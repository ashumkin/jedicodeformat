update IDE plugin's version info to Delphi 2005
compatible with Delphi 2005 update 2

File modified is JcfIdeMain.pas. Replace it and open the project JcfIde9.dpk, compile and install. Enjoy it. The modified file should be compatible to old projects for Delphi 5/6/7, since proper {$IFDEF } clauses are used. 

Change the required package from JvXXXD7R to JvXXXD9R.
Change the package name to JcfIde9.bpl.
Change the package description to 'JEDI Code Format IDE Plugin for Delphi 2005'.

In file JcfIdeMain.pas I change one procedure's implementation, because Delphi 2005 does not reload the files formatted automatically like Delphi 7. Also there are some lines I think are useless, I comment them out.

procedure TJcfIdeMain.DoFormatProject(Sender: TObject);
var
  lciProject: IOTAProject;
  lciModule:  IOTAModuleInfo;
  {$IFDEF VER170}lciAction: IOTAActionServices;{$ENDIF}
  liLoop:     integer;
  lsMsg:      string;
begin
  {$IFDEF VER170}lciAction := BorlandIDEServices as IOTAActionServices;{$ENDIF}
  lciProject := GetCurrentProject;
  if lciProject = nil then
    exit;

  lsMsg := 'JEDI Code Format of ' + lciProject.FileName + AnsiLineBreak +
    'Are you sure that you want to format all ' + IntToStr(lciProject.GetModuleCount) +
    ' files in the project.';

  if MessageDlg(lsMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;

  ClearToolMessages;
  //MakeEditorConverter; //Create an extra EditorConverter before calling its methods.
                       //Added to avoid runtime exception of access violation.
                       //Seems to be useless, since a FileConverter is acturally used here.
  //fcEditorConverter.BeforeConvert; //seems to be useless.

  { loop through all modules in the project }
  for liLoop := 0 to lciProject.GetModuleCount - 1 do
  begin
    lciModule := lciProject.GetModule(liLoop);
    FormatFile(lciModule.FileName);
    {$IFDEF VER170}lciAction.ReloadFile(lciModule.FileName);{$ENDIF}//reload the files.
  end;
  //fcEditorConverter.AfterConvert; //seems to be useless.
end; 
 