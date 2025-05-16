{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Frank Hoogerbeets

  Abstract:
    Project Metrics menu item under Project menu.
}
unit ProjectMetricsDlg;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, StrUtils,
  Grids, LCLProc, FileUtil, Clipbrd, ComCtrls, Types,
  TAGraph, TASeries, TASources, TAStyles, TATools, TAChartUtils, TATextElements;

const
  {$ifdef Unix}
    GRID_FONT_NAME = 'Liberation Mono';
    GRID_FONT_SIZE = 10;
  {$else}
    GRID_FONT_NAME = 'Lucida Console';
    GRID_FONT_SIZE = 9;
  {$endif}

type
  TSourceFile = record
    CodeLines: integer;
    CommentLines: integer;
    EmptyLines: integer;
    NonEmptyLines: integer;
  end;

  TProjectUnits = record
    PasFile: TSourceFile;
    LfmFile: TSourceFile;
    PasFilesTotal: TSourceFile;
    LfmFilesTotal: TSourceFile;
  end;

  { TfoProjectMetrics }

  TfoProjectMetrics = class(TForm)
    btClose: TButton;
    btCopyToClipboard: TButton;
    ChartToolset: TChartToolset;
    PanDragTool: TPanDragTool;
    ZoomDragTool: TZoomDragTool;
    MouseWheelZoomTool: TZoomMouseWheelTool;
    DataPointHintTool: TDataPointHintTool;
    lcsUsedUnits: TListChartSource;
    lcsInspectorUnits: TListChartSource;
    MetricsChart: TChart;
    MetricsBarSeries: TBarSeries;
    ChartStyles: TChartStyles;
    pcProjectMetrics: TPageControl;
    rbChart: TRadioButton;
    rbTable: TRadioButton;
    tsUsedUnits: TTabSheet;
    tsInspectorUnits: TTabSheet;
    sgUsedUnits: TStringGrid;
    sgInspectorUnits: TStringGrid;
    procedure btCloseClick(Sender: TObject);
    procedure btCopyToClipboardClick(Sender: TObject);
    procedure DataPointHintToolHint(ATool: TDataPointHintTool; const APoint:
      TPoint; var AHint: String);
    procedure DataPointHintToolHintLocation(ATool: TDataPointHintTool; AHintSize
      : TSize; var APoint: TPoint);
    procedure FormCreate(Sender: TObject);
    procedure GridPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure pcProjectMetricsChange(Sender: TObject);
    procedure rbTableChange(Sender: TObject);
  private
    CountLineStatus: integer;
    MetricsPerFile: TStringList;
    UsedUnits: TProjectUnits;
    InspectorUnits: TProjectUnits;
    procedure CountLinesInFile(const FileName: string; var Source: TSourceFile);
    procedure ScanProjectFile(const FileName: string; var prjUnits: TProjectUnits);
    procedure ScanUsedUnits(AList: TStrings);
    procedure ScanInspectorUnits(AList: TStrings);
    procedure GetResults(prjUnits: TProjectUnits; var grid: TStringGrid; chartSource: TListChartSource);
    procedure InitializeGrid(grid: TStringGrid);
    procedure InitializeChart;
  public
    procedure Analyze(AUnits, AInspectorUnits: TStrings);
  end;

implementation

{$R *.lfm}

{ TfoProjectMetrics }

procedure TfoProjectMetrics.CountLinesInFile(const FileName: string; var Source: TSourceFile);
var
  F: TextFile;
  Line: string;
  InCommentBlock: Boolean;
  TrimmedLine: string;
  i: Integer;
begin
  InCommentBlock := False;
  CountLineStatus := 0;

  try
    AssignFile(F, FileName);
    Reset(F);
    while not EOF(F) do
      begin
        ReadLn(F, Line);
        TrimmedLine := Trim(Line);

        if Trim(Line) = '' then
          begin
            Inc(Source.EmptyLines);
            Continue;
          end;

        Inc(Source.NonEmptyLines);

        if not InCommentBlock then
          begin
            i := Pos('//', TrimmedLine);
            if i > 0 then
              begin
                TrimmedLine := Trim(Copy(TrimmedLine, 1, i - 1));
                if TrimmedLine = '' then
                  Continue;
              end;
          end;

        if InCommentBlock then
          begin
            i := Pos('}', TrimmedLine);
            if i = 0 then
              i := Pos('*)', TrimmedLine);
            if i > 0 then
              begin
                InCommentBlock := False;
                TrimmedLine := Trim(Copy(TrimmedLine, i + 1, Length(TrimmedLine)));
                if TrimmedLine = '' then
                  Continue;
              end
            else
              Continue;
          end;

        i := Pos('{', TrimmedLine);
        if i = 0 then
          i := Pos('(*', TrimmedLine);
        if i > 0 then
          begin
            if Pos('}', TrimmedLine) > i then
              begin
                TrimmedLine := Trim(Copy(TrimmedLine, 1, i - 1) +
                                    Copy(TrimmedLine, Pos('}', TrimmedLine) + 1, Length(TrimmedLine)));
                if TrimmedLine = '' then
                  Continue;
              end
            else if Pos('*)', TrimmedLine) > i then
              begin
                TrimmedLine := Trim(Copy(TrimmedLine, 1, i - 1) +
                                    Copy(TrimmedLine, Pos('*)', TrimmedLine) + 1, Length(TrimmedLine)));
                if TrimmedLine = '' then
                  Continue;
              end
            else
              begin
                InCommentBlock := True;
                TrimmedLine := Trim(Copy(TrimmedLine, 1, i - 1));
                if TrimmedLine = '' then
                  Continue;
              end;
          end;

        if TrimmedLine <> '' then
          Inc(Source.CodeLines);
      end;
    if InCommentBlock then
      CountLineStatus := -2;
    CloseFile(F);
  except
    CountLineStatus := -1;
  end;
end;

procedure TfoProjectMetrics.ScanProjectFile(const FileName: string; var prjUnits: TProjectUnits);
begin
  with prjUnits do
    begin
      PasFile := default(TSourceFile);
      CountLinesInFile(FileName, PasFile);
      if CountLineStatus = -1 then
        MetricsPerFile.Add('Could not open file ' + FileName)
      else if CountLineStatus = -2 then
        MetricsPerFile.Add('Nested comment block found in ' + FileName);
      Inc(PasFilesTotal.NonEmptyLines, PasFile.NonEmptyLines);
      Inc(PasFilesTotal.EmptyLines, PasFile.EmptyLines);
      Inc(PasFilesTotal.CodeLines, PasFile.CodeLines);
      PasFile.CommentLines := PasFile.NonEmptyLines - PasFile.CodeLines;
      Inc(PasFilesTotal.CommentLines, PasFile.CommentLines);
      MetricsPerFile.Add(Format('%s,%d,%d,%d,%d',
        [ExtractFileName(FileName), PasFile.NonEmptyLines, PasFile.CodeLines, PasFile.CommentLines, PasFile.EmptyLines]));
    end;
end;

procedure TfoProjectMetrics.ScanUsedUnits(AList: TStrings);
var
  i: Integer;
begin
  InitializeGrid(sgUsedUnits);
  MetricsPerFile.Clear;

  if (AList = nil) or (AList.Count = 0) then
  begin
    sgUsedUnits.Cells[0, 1] := 'No active project.';
    exit;
  end;

  for i := 0 to AList.Count - 1 do
    ScanProjectFile(AList[i], UsedUnits);
  GetResults(UsedUnits, sgUsedUnits, lcsUsedUnits);
end;

procedure TfoProjectMetrics.ScanInspectorUnits(AList: TStrings);
var
  i: Integer;
begin
  InitializeGrid(sgInspectorUnits);
  MetricsPerFile.Clear;

  if (AList = nil) or (AList.Count = 0) then
  begin
    sgInspectorUnits.Cells[0, 1] := 'No active project.';
    exit;
  end;

  for i := 0 to AList.Count - 1 do
    ScanProjectFile(AList[i], InspectorUnits);
  GetResults(InspectorUnits, sgInspectorUnits, lcsInspectorUnits);
end;

procedure TfoProjectMetrics.InitializeGrid(grid: TStringGrid);
begin
  grid.ColCount := 7;
  grid.RowCount := 2;
  grid.FixedRows := 1;
  grid.FixedCols := 0;
  grid.Cells[0, 0] := 'File Name';
  grid.Cells[1, 0] := 'Non-Empty Lines';
  grid.Cells[2, 0] := 'Code Lines';
  grid.Cells[3, 0] := 'Comment Lines';
  grid.Cells[4, 0] := 'Empty Lines';
  grid.Cells[5, 0] := 'Comment %';
  grid.Cells[6, 0] := 'Empty %';
  grid.ColWidths[0] := 180; // Wider for file names
  grid.ColWidths[1] := 110;
  grid.ColWidths[2] := 110;
  grid.ColWidths[3] := 110;
  grid.ColWidths[4] := 110;
  grid.ColWidths[5] := 110;
  grid.ColWidths[6] := 110;
end;

procedure TfoProjectMetrics.GetResults(prjUnits: TProjectUnits;
  var grid: TStringGrid; chartSource: TListChartSource);
var
  i, j, Row: integer;
  FileMetrics: array of string;
  SummaryLines: integer;
begin
  with prjUnits do
    begin
      // Set grid row count: summary rows + file rows + header
      SummaryLines := 2; // For totals and percentages
      grid.RowCount := SummaryLines + MetricsPerFile.Count + 1; // +1 for header

      // Populate summary
      grid.Cells[0, 1] := 'Summary';
      grid.Cells[1, 1] := IntToStr(PasFilesTotal.NonEmptyLines);
      grid.Cells[2, 1] := IntToStr(PasFilesTotal.CodeLines);
      grid.Cells[3, 1] := IntToStr(PasFilesTotal.CommentLines);
      grid.Cells[4, 1] := IntToStr(PasFilesTotal.EmptyLines);
      if PasFilesTotal.NonEmptyLines > 0 then
        begin
          grid.Cells[5, 1] := FloatToStrF(PasFilesTotal.CommentLines / PasFilesTotal.NonEmptyLines * 100, ffFixed, 1, 2);
          grid.Cells[6, 1] := FloatToStrF(PasFilesTotal.EmptyLines / (PasFilesTotal.NonEmptyLines + PasFilesTotal.EmptyLines) * 100, ffFixed, 1, 2);
        end;

      // Prepare chart source
      chartSource.Clear;

      // Populate file metrics
      Row := SummaryLines + 1;
      j := 0;
      for i := 0 to MetricsPerFile.Count - 1 do
      begin
        if (MetricsPerFile[i].StartsWith('Could not open file') or
            MetricsPerFile[i].StartsWith('Nested comment block found')) then
        begin
          grid.Cells[0, Row] := MetricsPerFile[i];
          Inc(Row);
          Continue;
        end;
        FileMetrics := MetricsPerFile[i].Split([',']);
        if Length(FileMetrics) >= 5 then
        begin
          grid.Cells[0, Row] := FileMetrics[0]; // File Name
          grid.Cells[1, Row] := FileMetrics[1]; // Non-Empty Lines
          grid.Cells[2, Row] := FileMetrics[2]; // Code Lines
          grid.Cells[3, Row] := FileMetrics[3]; // Comment Lines
          grid.Cells[4, Row] := FileMetrics[4]; // Empty Lines
          if StrToIntDef(FileMetrics[1], 0) > 0 then
            begin
              grid.Cells[5, Row] := FloatToStrF(StrToIntDef(FileMetrics[3], 0) / StrToIntDef(FileMetrics[1], 1) * 100, ffFixed, 1, 2);
              grid.Cells[6, Row] := FloatToStrF(StrToIntDef(FileMetrics[4], 0) / (StrToIntDef(FileMetrics[1], 1) + StrToIntDef(FileMetrics[4], 1)) * 100, ffFixed, 1, 2);
            end;
          chartSource.AddXYList(
            j,
            [StrToInt(FileMetrics[2]), StrToInt(FileMetrics[3]), StrToInt(FileMetrics[4])],
            FileMetrics[0]
          );
          Inc(j);
          Inc(Row);
        end;
      end;
    end;
end;

procedure TfoProjectMetrics.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfoProjectMetrics.btCopyToClipboardClick(Sender: TObject);
var
  Grid: TStringGrid;
  i, j: integer;
  Line: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    if pcProjectMetrics.PageIndex = 0 then
      Grid := sgUsedUnits
    else
      Grid := sgInspectorUnits;

    // Copy header
    Line := '';
    for j := 0 to Grid.ColCount - 1 do
      if j = 0 then
        Line := Line + PadRight(Grid.Cells[j, 0], 30)
      else
        Line := Line + PadLeft(Grid.Cells[j, 0], 16);

    Lines.Add(Line);

    // Copy data rows
    for i := 1 to Grid.RowCount - 1 do
      begin
        Line := '';
        for j := 0 to Grid.ColCount - 1 do
          if j = 0 then
            Line := Line + PadRight(Grid.Cells[j, i], 30)
          else
            Line := Line + PadLeft(Grid.Cells[j, i], 16);
        Lines.Add(Line);
      end;

    Clipboard.AsText := Lines.Text;
  finally
    Lines.Free;
  end;
end;

procedure TfoProjectMetrics.DataPointHintToolHint(ATool: TDataPointHintTool;
  const APoint: TPoint; var AHint: String);
var
  ser: TBarSeries;
  nCode, nComments, nEmpty, nTotal: Double;
begin
  ser := ATool.Series as TBarSeries;
  nCode := ser.YValues[ATool.PointIndex, 0];
  nComments := ser.YValues[ATool.PointIndex, 1];
  nEmpty := ser.YValues[ATool.PointIndex, 2];
  nTotal := nCode + nComments + nEmpty;
  AHint := Format(
    '%s' + LineEnding +
    '  %.0f code lines (%.0f%%)' + LineEnding +
    '  %.0f comment lines (%.0f%%)' + LineEnding +
    '  %.0f empty lines (%.0f%%)', [
    ser.Source[ATool.PointIndex]^.Text,
    nCode, nCode/nTotal * 100,
    nComments, nComments/nTotal * 100,
    nEmpty, nEmpty/nTotal * 100
  ]);
end;

procedure TfoProjectMetrics.DataPointHintToolHintLocation(ATool:
  TDataPointHintTool; AHintSize: TSize; var APoint: TPoint);
begin
  APoint.Y := APoint.Y - AHintSize.CY;
end;

procedure TfoProjectMetrics.FormCreate(Sender: TObject);
begin
  sgUsedUnits.Font.Name := GRID_FONT_NAME;
  sgUsedUnits.Font.Size := GRID_FONT_SIZE;
  sgInspectorUnits.Font.Name := GRID_FONT_NAME;
  sgInspectorUnits.Font.Size := GRID_FONT_SIZE;
  pcProjectMetrics.ActivePageIndex := 0;
end;

procedure TfoProjectMetrics.GridPrepareCanvas(Sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
var
  grid: TStringGrid;
  textStyle: TTextStyle;
begin
exit;
  grid := Sender as TStringGrid;
  textStyle := grid.Canvas.TextStyle;
  if aCol > 0 then
    textStyle.Alignment := taRightJustify;
  grid.Canvas.TextStyle := textStyle;
end;

procedure TfoProjectMetrics.pcProjectMetricsChange(Sender: TObject);
begin
  MetricsChart.Parent := pcProjectMetrics.ActivePage;
  InitializeChart;
end;

procedure TfoProjectMetrics.rbTableChange(Sender: TObject);
begin
  InitializeChart;
end;

procedure TfoProjectMetrics.InitializeChart;
var
  grid: TStringGrid;
  ext: TDoubleRect;
begin
  case pcProjectMetrics.ActivePageIndex of
    0: begin
         MetricsBarSeries.Source := lcsUsedUnits;
         grid := sgUsedUnits;
       end;
    1: begin
         MetricsBarSeries.Source := lcsInspectorUnits;
         grid := sgInspectorUnits;
       end;
  end;
  MetricsChart.BottomAxis.Marks.Source := MetricsBarSeries.Source;
  MetricsChart.BottomAxis.Marks.Style := smsLabel;
  MetricsChart.Align := alClient;
  MetricsChart.Visible := rbChart.Checked;
  grid.Visible := rbTable.Checked;
  if MetricsBarSeries.Count > 20 then
  begin
    MetricsChart.BottomAxis.Marks.LabelFont.Orientation := 900;
    MetricsChart.BottomAxis.Marks.RotationCenter := rcEdge;
  end
  else if MetricsBarSeries.Count > 5 then
  begin
    MetricsChart.BottomAxis.Marks.LabelFont.Orientation := 450;
    MetricsChart.BottomAxis.Marks.RotationCenter := rcEdge;
  end else
  begin
    MetricsChart.BottomAxis.Marks.LabelFont.Orientation := 0;
    MetricsChart.BottomAxis.Marks.RotationCenter := rcCenter;
  end;
end;

procedure TfoProjectMetrics.Analyze(AUnits, AInspectorUnits: TStrings);
begin
  MetricsPerFile := TStringList.Create;
  try
    ScanUsedUnits(AUnits);
    ScanInspectorUnits(AInspectorUnits);
  finally
    MetricsPerFile.Free;
  end;
end;

end.
