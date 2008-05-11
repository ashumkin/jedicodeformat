unit JCFDllExtern;

interface

uses ConvertTypes;

const
  JCF_DLL_NAME = 'JCFEngine.dll';

{ format actions }
function JcfFormat(const psInput: string): string; stdcall; external JCF_DLL_NAME;
procedure JcfProcessFile(const psFileName: string); external JCF_DLL_NAME;
procedure JcfFormatFile(const psInputFileName, psOutputFileName: string); external JCF_DLL_NAME;

{ setup }
procedure JcfSetOnFormatStatusMessage(const pcProc: TStatusMessageProc); stdcall; external JCF_DLL_NAME;
procedure JcfSetOnFileFormatStatusMessage(const pcProc: TStatusMessageProc); stdcall; external JCF_DLL_NAME;
procedure JcfClearFormat; stdcall; external JCF_DLL_NAME;

procedure JcfReadRegistrySettings; stdcall; external JCF_DLL_NAME;
procedure JcfLoadDefaultSettingsFile; stdcall; external JCF_DLL_NAME;
procedure JcfLoadSettingsFile(const psFileName: string); stdcall; external JCF_DLL_NAME;

{ results of last format }
function JcfConvertError: Boolean; stdcall; external JCF_DLL_NAME;
function JcfTokenCount: integer; stdcall; external JCF_DLL_NAME;

function JcfFileConvertError: Boolean; stdcall; external JCF_DLL_NAME;
function JcfFileTokenCount: integer; stdcall; external JCF_DLL_NAME;


{ dialogs }
procedure JcfShowRegistrySettings; stdcall; external JCF_DLL_NAME;
procedure JcfShowFormatSettings; stdcall; external JCF_DLL_NAME;
procedure JcfShowAbout; stdcall; external JCF_DLL_NAME;

procedure JcfLogWrite(const ps: string); stdcall; external JCF_DLL_NAME;
procedure JcfCloseLog; stdcall; external JCF_DLL_NAME;
procedure JcfCheckShowLog; stdcall; external JCF_DLL_NAME;

implementation

end.
