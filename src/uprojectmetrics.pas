{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Frank Hoogerbeets

  Abstract:
    Project Metrics menu item under Project menu.
}
unit uProjectMetrics;

interface

uses Classes, SysUtils, LazIDEIntf, MenuIntf, ProjectIntf, ProjectMetricsDlg;

procedure Register;

implementation

procedure MyMenuItemClick(Sender: TObject);
var
  fo: TfoProjectMetrics;
  LazProject: TLazProject;
  LazFile: TLazProjectFile;
  Units: TStrings = nil;
  InspectorUnits: TStrings = nil;
  i: Integer;
begin
  fo := TfoProjectMetrics.Create(nil);
  try
    LazProject := LazarusIDE.ActiveProject;
    if LazProject <> nil then
    begin
      Units := LazarusIDE.FindUnitsOfOwner(LazProject, [fuooListed, fuooUsed]);
      InspectorUnits := TStringList.Create;
      for i := 0 to LazProject.FileCount - 1 do
      begin
        LazFile := LazProject.Files[i];
        if LazFile.IsPartOfProject then
          InspectorUnits.Add(LazProject.Files[i].FileName);
      end;
    end;
    fo.Analyze(Units, InspectorUnits);
    fo.ShowModal;
  finally
    InspectorUnits.Free;
    Units.Free;
    fo.Free;
  end;
end;

procedure Register;
var
  MenuItem: TIDEMenuCommand;
begin
  // Register the menu item under mnuProject (ideally under 'itmProjectOptions')
  MenuItem := RegisterIDEMenuCommand(mnuProject, 'ProjectMetrics', 'Project Metrics...');
  if Assigned(MenuItem) then
  begin
    // Using OnClickProc for Lazarus 3.2 compatibility
    MenuItem.OnClickProc := @MyMenuItemClick;
  end;
end;

end.
