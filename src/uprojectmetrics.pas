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

uses Classes, SysUtils, LazIDEIntf, MenuIntf, ProjectMetricsDlg;

procedure Register;

implementation

procedure MyMenuItemClick(Sender: TObject);
var
  fo: TfoProjectMetrics;
begin
  fo := TfoProjectMetrics.Create(nil);
  try
    fo.ShowModal;
  finally
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
