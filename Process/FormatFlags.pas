unit FormatFlags;

{ AFS 20 July 2001

  Code formatter exclusions flags
  These flags are used to switch off formatting based on special comments }

interface

type

  TFormatFlag = (eAllFormat,
    eAddSpace, eRemoveSpace,
    eAddReturn, eRemoveReturn,
    eAlignVars, eAlignConst, eAlignTypeDef, eAlignAssign,  eAlignComment,
    eCapsReservedWord, eCapsSpecificWord,
    eIndent, eLineBreaking, eBlockStyle,
    eWarning, eFindReplace, eFindReplaceUses);


  { these flags control:

    AllFormat: all clarify processes - turn the formatter as a whole on or off
    space: all processes that insert or remove spaces
    indent: inenting of code blocks etc
    return: all processes that insert or remove returns - note tat there is some overlap with
    eAlign: alignment of vars, assigns etc
    eLineBreaking: spliting long lines into 2 or more
    eBlockStyle - where to put begins & ends, else, etc
    eWarning: supress warnings
  }

  TFormatFlags = set of TFormatFlag;

{ read a comment for comment enabled flag data }
function ReadCommentJcfFlags(psComment: string;
  var psError: string;
  var peFlags: TFormatFlags;
  var pbOn: boolean): boolean;

const
  FORMAT_COMMENT_PREFIX = '//jcf:';
  FORMAT_COMMENT_PREFIX_LEN = 6;

  ALL_FLAGS: TFormatFlags = [Low(TFormatFlag)..High(TFormatFlag)];

implementation

uses
  { delphi } SysUtils,
  { Jcl} JclStrings, JclSysUtils;

type
  TRFlagNameData = record
    sName: string;
    eFlags: TFormatFlags;
  end;

const
  FORMAT_FLAG_NAMES: array[1..26] of TRFlagNameData =
  (
  (sName: 'format'; eFlags: [eAllFormat]),

  (sName: 'space'; eFlags: [eAddSpace, eRemoveSpace]),
  (sName: 'addspace'; eFlags: [eAddSpace]),
  (sName: 'removespace'; eFlags: [eRemoveSpace]),


  (sName: 'return'; eFlags: [eAddReturn, eRemoveReturn]),
  (sName: 'addreturn'; eFlags: [eAddReturn]),
  (sName: 'removereturn'; eFlags: [eRemoveReturn]),

  (sName: 'add'; eFlags: [eAddReturn, eAddSpace]),
  (sName: 'remove'; eFlags: [eRemoveReturn, eRemoveSpace]),


  (sName: 'align'; eFlags: [eAlignVars, eAlignConst, eAlignTypeDef, eAlignAssign, eAlignComment]),
  (sName: 'aligndef'; eFlags: [eAlignVars, eAlignConst, eAlignTypeDef]),
  (sName: 'alignfn'; eFlags: [eAlignVars, eAlignAssign]),

  (sName: 'alignvars'; eFlags: [eAlignVars]),
  (sName: 'alignconst'; eFlags: [eAlignConst]),
  (sName: 'aligntypedef'; eFlags: [eAlignTypeDef]),
  (sName: 'aligncomment'; eFlags: [eAlignComment]),

  (sName: 'alignassign'; eFlags: [eAlignAssign]),

  (sName: 'indent'; eFlags: [eIndent]),

  (sName: 'caps'; eFlags: [eCapsReservedWord, eCapsSpecificWord]),
  (sName: 'capsreservedwords'; eFlags: [eCapsReservedWord]),
  (sName: 'capsspecificword'; eFlags: [eCapsSpecificWord]),


  (sName: 'linebreaking'; eFlags: [eLineBreaking]),
  (sName: 'blockstyle'; eFlags: [eBlockStyle]),

  (sName: 'warnings'; eFlags: [eWarning]),
  (sName: 'findreplace'; eFlags: [eFindReplace]),
  (sName: 'findreplaceuses'; eFlags: [eFindReplaceUses])
  );


{ can stop and restart formating using these comments
 from DelForExp - Egbbert Van Nes's program }
const
  OLD_NOFORMAT_ON  = '{(*}';
  OLD_NOFORMAT_OFF = '{*)}';

  NOFORMAT_ON  = FORMAT_COMMENT_PREFIX + 'format=off';
  NOFORMAT_OFF = FORMAT_COMMENT_PREFIX + 'format=on';

{ like StrToBoolean, but recognises 'on' and 'off' too }
function LStrToBoolean(const ps: string): boolean;
begin
  if AnsiSameText(ps, 'on') then
    Result := True
  else if AnsiSameText(ps, 'off') then
    Result := False
  else
    Result := StrToBoolean(ps);
end;

{ this function works as follows
  Give it a comment text (psComment)
  and it returns
  - True if the comment is a special JCF flags comment
  psError is empty if the flags could be parsed, else contains an error message
  psFlags returns the set of flags referenced
  pbOn tells if they were turned on or off
}
function ReadCommentJcfFlags(psComment: string;
  var psError: string;
  var peFlags: TFormatFlags;
  var pbOn: boolean): boolean;
var
  lsPrefix, lsRest: string;
  lsSetting, lsState: string;
  lbFlagFound: Boolean;
  liLoop: integer;
begin
  Result := False;
  psError := '';

  // translate {(*} comments to jcf:format=on comments
  if psComment = OLD_NOFORMAT_ON then
    psComment := NOFORMAT_ON
  else if psComment = OLD_NOFORMAT_OFF then
    psComment := NOFORMAT_OFF;

  { all comments without the required prefix are of no import to this code
    if it's not one, then exit without error }
  lsPrefix := StrLeft(psComment, 6);
  if not (AnsiSameText(lsPrefix, FORMAT_COMMENT_PREFIX)) then
    exit;

  lsRest := Trim(StrRestOf(psComment, 7));

  { rest should read <setting>=<state>
    where the setting is one of the format flags, and the state is 'on' or 'off'
  }
  lsSetting := Trim(StrBefore('=', lsRest));
  lsState := Trim(StrAfter('=', lsRest));

  { is the comment well formed? }
  if (lsSetting = '') or (lsState = '') then
  begin
    psError := 'Comment ' + StrDoubleQuote(psComment) + ' has prefix but cannot be parsed';
    exit;
  end;

  { try and get a state flag from the string, abort if it fails }
  try
    pbOn := LStrToBoolean(lsState);
  except
    On EJclConversionError do
    begin
      psError := 'In comment ' + StrDoubleQuote(psComment) + ' , ' +
        ' state ' + StrDoubleQuote(lsState) + ' cannot be parsed to either on or off';
      exit;
    end
    else
      raise;
  end;

  // accept jcf:all=on to reset state to normal by removing all flags 
  if AnsiSameText(lsSetting, 'all') then
  begin
    peFlags := ALL_FLAGS;
    Result := True;
    exit;
  end;

  { match the setting from the table }
  lbFlagFound := False;
  for liLoop := low(FORMAT_FLAG_NAMES) to high(FORMAT_FLAG_NAMES) do
  begin
    if AnsiSameText(lsSetting, FORMAT_FLAG_NAMES[liLoop].sName) then
    begin
      peFlags := FORMAT_FLAG_NAMES[liLoop].eFlags;
      lbFlagFound := True;
      break;
    end;
  end;


  if not lbFlagFound then
  begin
    // unknown setting - nothing to do except log a message
    psError := 'In comment ' + StrDoubleQuote(psComment) + ' , ' +
      ' setting ' + StrDoubleQuote(lsSetting) + ' is not known';
    exit;
  end;
end;

end.
 