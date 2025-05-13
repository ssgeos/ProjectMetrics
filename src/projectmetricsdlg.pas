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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  LCLProc, FileUtil, LazIDEIntf, ProjectIntf, Clipbrd;

const
  {$ifdef Unix}
    MEMO_FONT_NAME = 'Liberation Mono';
    MEMO_FONT_SIZE = 11;
  {$else}
    MEMO_FONT_NAME = 'Lucida Console'; //'Courier New';
    MEMO_FONT_SIZE = 10;
  {$endif}

type
  TSourceFile = record
    CodeLines: integer;
    CommentLines: integer;
    EmptyLines: integer;
    NonEmptyLines: integer;   // code lines and comment lines
  end;

  type TProjectUnits = record
    PasFile: TSourceFile;
    LfmFile: TSourceFile;
    PasFilesTotal: TSourceFile;
    LfmFilesTotal: TSourceFile;
  end;

  { TfoProjectMetrics }

  TfoProjectMetrics = class(TForm)
    btClose: TButton;
    btCopyToClipboard: TButton;
    chWordWrap: TCheckBox;
    mmInspectorUnits: TMemo;
    mmUsedUnits: TMemo;
    pcProjectMetrics: TPageControl;
    tsUsedUnits: TTabSheet;
    tsInspectorUnits: TTabSheet;
    procedure btCloseClick(Sender: TObject);
    procedure btCopyToClipboardClick(Sender: TObject);
    procedure chWordWrapChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CountLineStatus: integer;
    MetricsPerFile: TStringBuilder;
    SummaryOfFiles: TStringBuilder;
    UsedUnits: TProjectUnits;
    InspectorUnits: TProjectUnits;
    procedure CountLinesInFile(const FileName: string; var Source: TSourceFile);
    procedure ScanProjectFile(const FileName: string; var prjUnits: TProjectUnits);
    procedure ScanUsedUnits;
    procedure ScanInspectorUnits;
    procedure GetResults(prjUnits: TProjectUnits; var memo: TMemo);
  public
  end;


  //foProjectMetrics: TfoProjectMetrics;


implementation

{$R *.lfm}

{ TfoProjectMetrics }

procedure TfoProjectMetrics.CountLinesInFile(const FileName: string; var Source: TSourceFile);
// count empty lines, code lines and total lines
// CountLineStatus is set:
// -1: error opening file
// -2: nested comment block found
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

        // Check for single-line // comment
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

        // Handle multi-line comment blocks
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

        // Check for start of multi-line comment block
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

        // Count non-empty, non-comment lines
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
      // count lines per file
      PasFile := default(TSourceFile);
      CountLinesInFile(FileName, PasFile);
      if CountLineStatus = -1 then
        MetricsPerFile.Append('Could not open file ' + FileName)
          .AppendLine
      else if CountLineStatus = -2 then
        MetricsPerFile.Append('Nested comment block found in ' + FileName)
          .AppendLine;
      // cumulative
      Inc(PasFilesTotal.NonEmptyLines, PasFile.NonEmptyLines);
      Inc(PasFilesTotal.EmptyLines, PasFile.EmptyLines);
      Inc(PasFilesTotal.CodeLines, PasFile.CodeLines);
      PasFile.CommentLines := PasFile.NonEmptyLines - PasFile.CodeLines;
      Inc(PasFilesTotal.CommentLines, PasFile.CommentLines);
      // metrics per file
      MetricsPerFile.Append(FileName
        + ', Total Lines (non-empty): '
        + IntToStr(PasFile.NonEmptyLines)
        + ', Code Lines: '
        + IntToStr(PasFile.CodeLines)
        + ', Comment Lines: '
        + IntToStr(PasFile.CommentLines)
        + ', Empty Lines: '
        + IntToStr(PasFile.EmptyLines))
        .AppendLine.AppendLine;
    end;

  {
  else if Ext = '.lfm' then
    begin
      LfmFile := default(TSource);
      CountLinesInFile(FilePath, LfmFile);
      Inc(LfmFilesTotal.TotalLines, LfmFile.TotalLines);
      Inc(LfmFilesTotal.EmptyLines, LfmFile.EmptyLines);
      Writeln(
        'File: ', SearchRec.Name,
        ', Total Lines: ', LfmFile.TotalLines,
        ', Empty Lines: ', LfmFile.EmptyLines
      );
    end;
  }
end;

procedure TfoProjectMetrics.ScanUsedUnits;
var
  LazProject: TLazProject;
  Units: TStrings;
  i: integer;
begin
  mmUsedUnits.Clear;
  MetricsPerFile.Clear;
  SummaryOfFiles.Clear;
  LazProject := LazarusIDE.ActiveProject;
  if LazProject <> nil then
    begin
      Units := LazarusIDE.FindUnitsOfOwner(LazProject, [fuooListed, fuooUsed]);
      try
        for i := 0 to Units.Count - 1 do
          ScanProjectFile(Units[i], UsedUnits);
        GetResults(UsedUnits, mmUsedUnits);
      finally
        Units.Free;
      end;
    end
  else
    mmUsedUnits.Lines.Add('No active project.');
end;

procedure TfoProjectMetrics.ScanInspectorUnits;
var
  LazProject: TLazProject;
  i: integer;
  LazFile: TLazProjectFile;
begin
  mmInspectorUnits.Clear;
  MetricsPerFile.Clear;
  SummaryOfFiles.Clear;
  LazProject := LazarusIDE.ActiveProject;
  if LazProject <> nil then
    begin
      for i := 0 to LazProject.FileCount - 1 do
        begin
          LazFile := LazProject.Files[i];
          if LazFile.IsPartOfProject then //and FileNameIsPascalUnit(LazFile.Filename) then
            ScanProjectFile(LazFile.FileName, InspectorUnits);
        end;
      GetResults(InspectorUnits, mmInspectorUnits);
    end
  else
    mmInspectorUnits.Lines.Add('No active project.');
end;

procedure TfoProjectMetrics.GetResults(prjUnits: TProjectUnits; var memo: TMemo);
begin
  with prjUnits do
    begin
      SummaryOfFiles.Append(
        'Total lines (non-empty): '
        + IntToStr(PasFilesTotal.NonEmptyLines))
      .AppendLine.Append(
        'Total code lines (non-comment, non-empty): '
        + IntToStr(PasFilesTotal.CodeLines))
      .AppendLine.Append(
        'Total comment lines (incl. blocks): '
        + IntToStr(PasFilesTotal.CommentLines))
      .AppendLine.Append(
        'Total empty lines: '
        +IntToStr(PasFilesTotal.EmptyLines))
      .AppendLine.AppendLine;
      // prevent div by zero
      if PasFilesTotal.NonEmptyLines > 0 then
        begin
          SummaryOfFiles.Append(
            'Comment lines percentage (of non-empty): '
            + FloatToStrF(PasFilesTotal.CommentLines / PasFilesTotal.NonEmptyLines * 100, ffFixed, 1, 2) + '%')
          .AppendLine.Append(
            'Empty lines percentage (of total): '
            + FloatToStrF(PasFilesTotal.EmptyLines / (PasFilesTotal.NonEmptyLines + PasFilesTotal.EmptyLines) * 100, ffFixed, 1, 2) + '%')
          .AppendLine
          .AppendLine
          .AppendLine
          .Append('Metrics per file:')
          .AppendLine
          .AppendLine;
        end;
      memo.Text := SummaryOfFiles.ToString + MetricsPerFile.ToString;
    end;
end;

procedure TfoProjectMetrics.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfoProjectMetrics.btCopyToClipboardClick(Sender: TObject);
begin
  if pcProjectMetrics.PageIndex = 0 then
    Clipboard.AsText := mmUsedUnits.Text
  else
    Clipboard.AsText := mmInspectorUnits.Text;
end;

procedure TfoProjectMetrics.chWordWrapChange(Sender: TObject);
begin
  mmUsedUnits.WordWrap := chWordWrap.Checked;
  mmInspectorUnits.WordWrap := chWordWrap.Checked;
end;

procedure TfoProjectMetrics.FormCreate(Sender: TObject);
begin
  mmUsedUnits.Font.Name := MEMO_FONT_NAME;
  mmUsedUnits.Font.Size := MEMO_FONT_SIZE;
  mmInspectorUnits.Font.Name := MEMO_FONT_NAME;
  mmInspectorUnits.Font.Size := MEMO_FONT_SIZE;

  MetricsPerFile := TStringBuilder.Create(1024);
  SummaryOfFiles := TstringBuilder.Create(256);

  pcProjectMetrics.ActivePageIndex := 0;

  try
    ScanUsedUnits;
    ScanInspectorUnits;
  finally
    MetricsPerFile.Free;
    SummaryOfFiles.Free;
  end;
end;

end.

