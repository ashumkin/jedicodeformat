HelpScribble project file.
12
...
0
1
JEDI Delphi Code Format



TRUE

D:\Daten\Sonstige\CODEFO~3\Images
1
BrowseButtons()
0
FALSE

FALSE
TRUE
16777215
0
16711680
8388736
255
FALSE
FALSE
FALSE
23
10
Scribble10
Main
Jedi Code Format; Welcome



Writing



FALSE
41
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Jedi Code Format V\lang2057\f1 2.0 Beta 6\cf0\lang1031\b0\f0\fs20 
\par \cf2\{keepn\}
\par \cf0 
\par This is the main documentation for the \lang2057\f1 Jedi Code Formatter\lang1031\f0 .\lang2057\f1  \lang1031\f0 The formatter can standardise all aspects of Delphi Object Pascal source code formatting, including indentation, spacing and capitalisation\lang2057\f1 .\lang1031\f0  
\par 
\par \lang2057\f1 Not every \lang1031\f0 control\lang2057\f1  on the configuration is documented\lang1031\f0 . In many cases the control's captions should be self-explanatory.
\par 
\par This helpfile contains help on the the following topics:
\par \cf3\lang2057\strike\f1 
\par Programs\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 12\lang1031\f0\}
\par \cf0 
\par \cf3\lang2057\strike\f1 Settings storage\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 15\lang1031\f0\}\cf0 
\par \cf3\lang2057\strike\f1 Registry\lang1031\f0  settings\cf2\strike0\{linkID=\lang2057\f1 20\lang1031\f0\}
\par \lang2057\f1\tab\cf3\strike Logging\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 24\lang1031\f0\}
\par \lang2057\f1\tab\cf3\strike Exclusions\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 26\lang1031\f0\}
\par \cf0 
\par \cf3\lang2057\strike\f1 Basic Settings\cf2\lang1031\strike0\f0\{linkID=30\}\cf0 
\par \cf3\strike Obfuscate Settings\cf2\strike0\{linkID=50\}\cf0 
\par \cf3\strike Clarify Settings\cf2\strike0\{linkID=60\}
\par \lang2057\f1\tab\cf3\strike Spaces\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 7\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Indentation\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 8\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Long lines\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 85\lang1031\f0\}
\par \lang2057\f1\tab\cf3\strike Returns\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 9\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Blocks\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 10\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Align\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 11\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Capitalisation\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 12\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Find and replace\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 13\lang1031\f0 0\}
\par \lang2057\f1\tab\cf3\strike Find and replace on uses\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 14\lang1031\f0 0\}
\par \cf0 
\par \cf3\lang3079\strike\f1 Command line parameters\cf2\strike0\{linkID=200\}
\par \cf3\strike Special Comments\cf2\lang1031\strike0\f0\{linkID=210\}
\par \cf0 
\par Th\f1 e newest version of the Jedi Code Format can always be found at \f0 
\par \cf1\strike\f2 http://jedicodeformat.sourceforge.net/\cf2\strike0\{link=*! ExecFile("http://jedicodeformat.sourceforge.net/")\}\cf0\f0 
\par \f1 
\par Helpfile by \cf3\strike Ralf Steinhaeusser\cf2\strike0\{linkID=1000\}
\par 
\par \cf0\f3 Last change : August 18th, 2003\f1 
\par }
12
Scribble12
Programs




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Programs\cf2\b0\f1\fs20 
\par 
\par \f0 All of these program contain the same code formatting engine with different interfaces.\f1 
\par 
\par \b\f0 jcf.exe\b0  - command-line program suitable for batch files and old-school unix programmers who like command-line programs. The  functionality is much like \b JcfGui.exe\b0 , i.e. it can work on a file, a directory or a directy tree, and can format in place, or to an output file.
\par 
\par \b jcfGui.exe\b0  - a GUI interface to formatting files and directories. Best used when you want to interactively format a file, directory or directory tree.
\par 
\par \b JcfIde7.bpl\b0  - the Delphi IDE plug-in compiled for Delphi 7. Requires JCL and JCVL. This is the easiest way to use the formatter. Just press a button and watch your code change. the other advantage is that if you don't like the results, by clicking "undo" twice immediately you can undo the format.
\par 
\par If anyone can compile this for other versions of Delphi I will gladly upload the binaries to Sourceforge.
\par 
\par \b JcfNotepad.exe\b0  is a new gui - for experimenting with source and layout. Input source is loaded/typed/pasted into the input text area, and the formatter output is generated to the output text area.\f1 
\par }
15
Scribble15
Settings stored in registry and file




Writing



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fnil\fcharset0 Courier New;}{\f4\fswiss Arial;}{\f5\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Settings stored in registry and file\cf2\b0\f1\fs20 
\par 
\par \cf0\f2 Some settings are stored in the \b registry \b0 under\f3  \lang1031 HKEY_CURRENT_USER\\Software\\Jedi\\JediCodeFormat\f2 . These are per-user settings, mostly UI settings like recently used files, logging options, showing parse tree on error. The path to the configuration file is stored here.
\par 
\par \f4 
\par \f2 The \b configuration file\b0  has a default name of \f3 JcfSettings2.cfg\f2 . This file stores all the settings of how to format delphi source. The advantages of keeping this data in a file, not in the registry, are:
\par \pard{\pntext\f5\'B7\tab}{\*\pn\pnlvlblt\pnf5\pnindent0{\pntxtb\'B7}}\fi240\li140\tx180 You can keep multiple format profiles by keeping multiple files
\par {\pntext\f5\'B7\tab}Files can be exchanged by email and so on, and can be edited in notepad
\par {\pntext\f5\'B7\tab}You and your colleages can all use the same settings file, if you keep it on a network drive.
\par {\pntext\f5\'B7\tab}\pard\li380\tx180 
\par \pard\tx180 The configuration file format is inspired by XML, but I don't know enough XML and haven't found a reason yet to investigate if it is fully XML compliant or not. However, if you can edit plain text and undertsand tags inside "<" and ">" then the format shouldn't be difficult.
\par 
\par \pard\cf2\lang2057\f1 
\par }
20
Scribble20
General Registry Settings




Writing



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Registry Settings\cf2\b0\f1\fs20 
\par 
\par \b\f0 Convert settings file:\b0  The path to the settings file.
\par 
\par \b MRU max items:\b0  The number of items stored in the Most-Recently Used list of the File menu. When this number is exceeded, the bottom item, ie the least recently used file, will be removed.
\par 
\par \b Show parse tree:\b0  JCF 2.0 generates a full parse tree of the input source file. Showing this can be useful for debugging. I recommend that you set this to "on error", which will show it only when the source cannot be parsed. The last token of the parse tree then will give you some idea of what the parser thought was going on.
\par \f1 
\par }
24
Scribble24
Logging Registry Settings
Logging settings



Writing



FALSE
24
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Logging Registry Settings\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 30.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par The log is a text file that is written every time that the code formatter is run, detailing what it did in the run. It can useful to you as a user when 
\par 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent0{\pntxtb\'B7}}\fi-200\li200 You have just run a batch and want to see how many and which files were formatted.
\par {\pntext\f2\'B7\tab}The run's warnings are listed in the log file.
\par {\pntext\f2\'B7\tab}\pard 
\par The log is also useful for debugging, as you can write to it from in the program. 
\par 
\par \b Log file detail level:\b0 
\par \pard{\pntext\f2\'B7\tab}{\*\pn\pnlvlblt\pnf2\pnindent200{\pntxtb\'B7}}\fi-200\li200 Error: write an entry into the log only when an error occurs or a warning is generated. Summary entries at the end of the run are also written.
\par {\pntext\f2\'B7\tab}File: As error, also write a line for each file that is processed.
\par {\pntext\f2\'B7\tab}Token: As File, also write an entry for each token in each file that is processed. This can be extremely verbose, and is useful only for debugging.
\par {\pntext\f2\'B7\tab}\pard 
\par \b Backup file extension:\b0  The extension for backup files (default .bak).
\par \b Output file extension:\b0  The extension for backup files (default .out)
\par \b Log file directory.\b0  The directory where the log file (called CodeFormat.log)\f1  \f0 is generated. The system temp, application or a user specified directory can\f1  \f0 be used. If you want to see your warnings, look in the log.
\par 
\par }
26
Scribble26
Exclusions Registry Settings
Exclusions



Writing



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}{\f3\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Exclusions Registry Settings\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par Files and directories with names listed here will be excluded when a directory\lang3079\f1  \lang1031\f0 or directory tree is processed.
\par 
\par A list of files can be entered. Do not enter path or extension, 
\par e.g. \b\f2 MyUnit\b0\f0  not \f2 c:\\code\\MyDelphiProject\\MyUnit.pas\f0  
\par 
\par These files will not be processed in a batch run that includes their directory. This is particularly useful,
\par for e.g. in a project that uses COM and type libraries. the \f3 .pas\f0  files generated from the type libraries can be extremely large, and are regenerated by the compiler, so reformatting them is a waste of time.
\par 
\par }
30
Scribble30
Basic settings
Basic settings



Writing



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset0 Courier New;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Basic settings\cf0\b0\fs20 
\par \cf2\{keepn\}
\par 
\par \{bmc 20.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par \lang2057\f1 These are found on the main form of the JCFGui program.
\par \lang1031\f0 
\par \b Mode:\b0  "Obfuscate" means to make unclear. You will want clarify not obfuscate.\lang2057\f1  \lang1031\f0 I did the obfuscation partly out of curiosity, partly as it is simple, and mostly to generate worst-case test units. It is not useful for production code, except to see how bad code layout can be but still compile.
\par 
\par \b Backup:\b0  I use "output to separate file" for testing.\lang2057\f1  The input file is left intact, with the output file going to a new file, with the same directory and the same name, but a different extension, typically \f2 .out\f1 . 
\par 
\par For production use, I recommend that you use "backup to seperate file". The input file is formatted, but a copy of the prior file is kept, with the sme directory and name, but a different extension, typically \f2 .bak\f3 . If you really trust your source control system, you may want to use "no backup".\lang1031\f0 
\par 
\par See the \lang2057\f1 logging registry\lang1031\f0  settings to change these extensions.
\par 
\par }
40
Scribble40
Format file




Writing



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Format file
\par 
\par \cf2\b0\fs20 This page describes the format file location, if it is read-only, when it was written and by which version of the formatter. In the Description field you can describe the file.
\par \f1 
\par }
50
Scribble50
Obfuscate Settings
Obfuscate Settings



Writing



FALSE
26
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Obfuscate Settings\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 50.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par These control the obfuscate process. This mode does everything wrong short of
\par making semantic changes to your program. In spite of being unreadable, it will still
\par compile as before.
\par 
\par \b Obfuscate word caps:\b0  Delphi is not case sensitive, so case can be removed
\par without affecting the compilation of your program.
\par \b Remove white space:\b0  All unnecessary white space (i.e. almost all of it) will be
\par removed.
\par \b Remove comments:\b0  removes all comments, be they \{\} (* *) or // comments.
\par Exceptions are mode for comments that contain compiler directives, code
\par formatter directives or Delphi .dfm
\par form magic.
\par \b Remove indent:\b0  to see this work, turn off "Remove white space" and
\par "rebreak lines". All statements will come out as flush left.
\par \b Rebreak lines:\b0  Utterly destroy your formatting by putting us much text as
\par possible on each line. All existing returns will be removed and new ones
\par inserted to produce lines of roughly equal length. The Maximum line length on the clarify
\par settings is used.
\par }
60
Scribble60
Clarify Settings
Clarify Settings



Writing



FALSE
31
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset0 Courier New;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Clarify Settings\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \b Show warnings:\b0  Puts a message to the log file when certain conditions in the code are\f1  \f0 encountered that are possibly dangerous or obsolete and deprecated, but are not\f1  \f0 picked up by the Delphi Compiler. In the IDE plug-in, these are written to the\f1  \f0 IDE message window.
\par 
\par The goal with warnings is not to duplicate any message that the Delphi compiler produces, but to flag other conditions that might be hazardous, and leave it up to the user to see of they actually warrant attention. The following warnings are implemented:
\par 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200 Use of the floating point types \f2 real \f0 and \f2 real48\f0 . These types are included only for backward compatibility and are both slower and less precise than the \f2 double \f0 type. See the Delphi help under "real types" for more details.
\par {\pntext\f3\'B7\tab}A \f2 case \f0 block with no else case is a common cause of error. If you never expect the else case to be reached, rather than leaving it out, code the else case to \f2 assert(false, "should not be here");\f0  or throw an exception. You never know - changes later in the code (e.g. a new item in an enumeration or a new integer code value) could be introduced into the program later, and rather than do nothing silently, you should rather notify yourself that your assumptions are no longer valid.
\par {\pntext\f3\'B7\tab}Call to \f2 AnObject.Destroy\f0 . This is never needed in practice. Call \f2 AnObject.Free\f0  or even better, \f2 FreeAndNil(AnObject)\f0 .
\par {\pntext\f3\'B7\tab}Empty block, a\f2  begin ... end\f0  pair with nothing between them is a placeholder for code yet to be written and should be filled or removed.
\par {\pntext\f3\'B7\tab}Assignment to the function name. This is an obsolete style. Rather use the \f2 Result \f0 variable.
\par {\pntext\f3\'B7\tab}\pard 
\par \b Run plug-ins:\b0  A flag for user code to hook into. If you write a custom processor,\f1  \f0 use\lang2057\f1  \lang1031\f0 this flag to turn it on and off.
\par 
\par \f1 The clarifying algorithm can be configured via the following pages:\f0 
\par 
\par \cf3\strike Spaces\cf2\strike0\{linkID=70\}\cf0 
\par \cf3\strike Indentation\cf2\strike0\{linkID=80\}\cf0 
\par \cf3\lang2057\strike\f1 Long Lines\cf2\lang1031\strike0\f0\{linkID=\lang2057\f1 85\lang1031\f0\}\cf0 
\par \cf3\strike Returns\cf2\strike0\{linkID=90\}\cf0 
\par \cf3\strike Blocks\cf2\strike0\{linkID=100\}\cf0 
\par \cf3\strike Align\cf2\strike0\{linkID=110\}\cf0 
\par \cf3\strike Capitalisation\cf2\strike0\{linkID=120\}\cf0 
\par \cf3\strike Find and replace\cf2\strike0\{linkID=130\}\cf0 
\par \cf3\strike Find and replace on uses\cf2\strike0\{linkID=140\}\cf0 
\par 
\par 
\par }
70
Scribble70
Clarify - Spaces
Clarify - Spaces



Writing



FALSE
32
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\f0\fs32\{linkID=\f1 %\f0 60\}\cf1  Spaces\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 70.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par \b Fix spacing:\b0  This turns on a set of rules that set a single space, or no
\par space before or after certain tokens. For e.g., There is supposed to be no space
\par between a procedure call and he bracket that starts its parameter list.
\par 
\par \b Space before class heritage:\b0  A very specific option, put in on request. When
\par enabled it will format a class headers as 
\par \f2 TFoo = class (TBar)\f0  not
\par \f2 TFoo = class(TBar)\f0 
\par 
\par \b Spaces before colon:\b0  these options determine the number of spaces inserted
\par before colons in various contexts. Borland standard style is to always put zero
\par spaces before a colon, but some people like these.
\par E.g. setting \b Spaces before colon in var and const declaration\b0  to 3 would change 
\par \f2   FooStr: array[1..2] of string = ('hi', 'lo');
\par \f0 to
\par \f2   FooStr   : array[1..2] of string = ('hi', 'lo');
\par \f0 
\par 
\par \b Turn tabs to spaces:\b0  Recommended. If you change the "block indentation
\par spaces", you may want to change the "spaces per tab" to match.
\par 
\par \b Turn spaces to tabs :\b0  The opposite of the above. Not recommended, unless you
\par really like tabs for some reason.
\par 
\par }
80
Scribble80
Clarify - Indentation
Clarify - Indentation



Writing



FALSE
53
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Indentation\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 80.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par \i Indent:\i0  Do block indentation on global types and vars, class declarations and procedure bodies. The number of spaces per indentation level can be set, but 2 is standard. You can decide if you want this on in globals (i.e. variables, constants and types), in class declarations and in procedure bodies.
\par 
\par \b Extra indent for begin and end in procedures:\b0  Turn this option on and specify the number of spaces if you like the variation in formatting that indents a block begin and end a few spaces more then the statement that owns them. 
\par E.g. turning this option on and setting the number of extra spaces to 4 (block indent is still 2) produces output like:
\par 
\par \b\f2 procedure \b0 someproc;
\par \b begin\b0 
\par   \b if \b0 condition \b then
\par       begin\b0  // note the extra indent here and on the end statement
\par     statements;
\par       \b end\b0 
\par   \b else\b0 
\par     statements;
\par \b end\b0 ;
\par 
\par \b\f0 Different indent for first level:\b0  Turn this on to override the indent spaces for the first level of indentation. This allows indent styles, for e.g. where the block indent is 4, but the first indent is at 2 spaces in. Subsequent indents will be relative to this level.
\par A level of 5 would produce an output like this:
\par 
\par \f2 type 
\par      TFooApp = object(TApplication)
\par        procedure HandleEvent(var Event: TEvent); virtual;
\par ...
\par procedure FooFunc(bla: longint);
\par var
\par      I: integer;
\par begin
\par      STR(Zahl, ZWS);
\par \f0 
\par 
\par \b Keep single-line comments with code:\b0  These options will move single-line comments to start at the same indent level as the surrounding code.
\par 
\par \b Remove returns in variable declarations :\b0  Setting this option to true converts\lang2057\f1 , for e.g.\lang1031\f0 
\par 
\par \f2 var
\par   Num: word;
\par   Zun, 
\par   Vorn: NStr;
\par \f0 
\par to
\par \f2 
\par var
\par   Num: word;
\par   Zun, Vorn: NStr;
\par \f0 
\par 
\par }
85
Scribble85
Clarify: Long Lines




Writing



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fswiss Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Clarify: Long Lines\cf2\b0\f1\fs20 
\par 
\par \cf0\b\f2 B\lang1031\f3 reak l\lang2057\f2 ines that are longer than the max line length\lang1031\f3 :\b0  A line longer than the \lang2057\b\f2 max line length \lang1031\b0\f3 threshold (the standards recommend 80 chars) will be broken if possible.\lang2057\f2 Each place on the line between source tokens is given a score\lang1031\f3  \lang2057\f2 based on how \lang1031\f3 aesthetically pleasing\lang2057\f2  it would be to break the line there.
\par \lang1031\f3 
\par \lang2057\b\f2 Usually:\b0   Break the line at the best place, if any place at all is found where the line can be broken.\b 
\par Sometimes\b0 : Break the line at the best place, provided that the best place is good enough. This option is recommended, as it means that you should never have a return inserted in an ugly place just for the sake of breaking a line.\cf2\f1 
\par }
90
Scribble90
Clarify - Returns
Clarify - Returns



Writing



FALSE
92
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset0 Courier New;}{\f3\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Returns\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 90.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par \lang2057\f1 
\par \lang1031\b\f0 Insert returns, remove returns:\b0  These turn on a rules that set a single new line, double\lang3079\f1  \lang1031\f0 new line or no new line before or after certain tokens. For e.g., There is\lang3079\f1  \lang1031\f0 supposed to be a double new line after the \b\f2 interface \b0\f0 and \b\f2 implementation \b0\f0 keywords, so this is one of the misc. good places to have returns.
\par 
\par \b Remove returns in expressions:\b0  Return removal is often an aesthetic choice. When coding an long expression, you may be in the habit in breaking the line in the place most aesthetically pleasing pace to you, e.g.
\par 
\par \f2 aValue := SomeThing + SomethingElse - SomethingOrOther -
\par    OtherStuff + OtherMoreStuff;
\par \f0 
\par Here the line break is used to emphasise the difference between the 'something' terms and the 'other' terms. If you do this, then turn off "Remove returns in expressions". If you don't, then turn it on, and let this process remove all the returns in your expression. Then the line breaker will rebreak the long expressions at points that it considers good based on a purely syntactic analysis of the structure of the expression.
\par 
\par \b Remove returns at start and end of block:\b0  Removes blank lines just before \b\f2 end \b0\f0 or just after \b\f2 begin \b0\f0 e.g.  will turn
\par 
\par \f3 if a > 3 then
\par begin
\par 
\par   b := 4;
\par end;
\par 
\par \f0 to
\par 
\par \f3 if a > 3 then
\par begin
\par   b := 4;
\par end;
\par 
\par \b\f0 Remove blank lines in procedure var section:\b0  e.g. turns 
\par 
\par \f3  procedure foo;
\par  var
\par   a: integer;
\par 
\par   b: string;
\par begin
\par 
\par \f0 to
\par 
\par \f3 procedure
\par var
\par   a: integer;
\par   b: string;
\par begin
\par 
\par \f0 Remove returns after the procedure header, but before the opening begin or var/const/type
\par declarations.
\par  i.e.remove them here:
\par 
\par \f3 procedure foo;
\par 
\par begin
\par \f0 
\par and here:
\par 
\par \f3 procedure bar;
\par 
\par var
\par \f0 
\par \b 
\par Remove \f1 returns in variable declaration\f0 :\b0  e.g. turns 
\par 
\par \f3  var
\par   a: integer;
\par 
\par   b: string;
\par \f0 
\par 
\par \f1 to
\par 
\par \f3  var
\par   a: integer;
\par   b: string;
\par \f0 
\par \b Number of returns after the final end:\b0  This setting standardises the way that
\par the unit ends. The default value of 1 return after the end of the unit's text
\par should please most people, but other values (for e.g. zero, or two) can be entered.
\par The allowable range is 0 to an arbitrarily chosen maximum of 255, though IMHO
\par anything above 2 is just silly.
\par 
\par \b Return chars:\b0  Use this setting if your file case come from a different OS and
\par you are having trouble viewing it as the return chars don't seem to be working
\par properly. 
\par Unix system such as Linux store returns differently to DOS/Windows machines .
\par Unix uses just a Linefeed character (#10), and Windows uses Carriage Return +
\par Linefeed chars (#10#13). This setting will standardise all returns. But mostly
\par you won't need to, so leave it as "leave as is"
\par 
\par }
100
Scribble100
Clarify - Blocks
Clarify - Blocks



Writing



FALSE
37
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Blocks\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 100.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par A block is a group of statements,  either grouped inside a begin..end. I have also used this term to refer to circumstances where there is no begin .. end block, but a statement is owned by a compound statement, e.g an if or while statement. These options give you control over when to use a new line in blocks and owned statements.
\par 
\par  E.g 1).
\par 
\par You may want to emulate the style of end-line blocks that is for popular in C and Java programming, i.e.
\par 
\par \f2   if somecond then begin
\par     statemements
\par   end
\par 
\par \f0 In this case, you would set the "block with begin" new line style to "never"
\par 
\par E.g 2)
\par 
\par  You may sensibly decide that blocks with begin should be as follows:
\par 
\par \f2 if somecond then
\par begin
\par   statemements
\par end
\par \f0 
\par But that if you code \f2 if a > 3 then b := 4;\f0  you do not wish the code formatter to drop \f2 b := 4\f0  onto a new line in cases where you wrote it all on one line.
\par  
\par In this case, you would set the "\b block with begin\b0 " new line style
\par to "always", and the "\b block without begin\b0 " new line style to "leave as is".
\par 
\par Labels are a little-used feature. they are the target of goto statements. See
\par the Delphi help under goto for what they are.
\par 
\par }
110
Scribble110
Clarify - Align
Clarify - Align



Writing



FALSE
63
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fnil\fcharset0 Courier New;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Align\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 110.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par These processes make you code look pretty by lining up code elements on successive lines. For e.g. this turns
\par 
\par \b\f2 var\b0 
\par  a: integer;
\par  bee: string;
\par \b begin\b0 
\par  a := 2; // a comment
\par  bee := 'three'; // another comment
\par \f0 
\par 
\par into
\par 
\par  
\par \b\f2 var\b0 
\par  a:   integer;
\par  bee: string;
\par \b begin\b0 
\par  a   := 2;       // a comment
\par  bee := 'three'; // anther comment
\par \f0 
\par I used to think that this was silly - too much manual labour for no benefit. Then I got the program to do it for me. Now I think it looks neat. Alignment is turned off by default as it is not part of the Borland code formatting standard.
\par 
\par The following code elements can be aligned:
\par 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200 Assign: aligns the := of assignment statements in procedures.
\par {\pntext\f3\'B7\tab}Const: aligns the = sign of const declarations.
\par {\pntext\f3\'B7\tab}Var: aligns the type names of variable declarations.
\par {\pntext\f3\'B7\tab}Typedef Aligns the = sign of type declarations.
\par {\pntext\f3\'B7\tab}Comments: Aligns the start of end line comments.
\par {\pntext\f3\'B7\tab}\pard 
\par The column of alignment will never be less than the min column or more then the max column. Use these parameters to stop long statements all ending up aligned in a column far to the right. The Max variance controls how much variation in length is tolerated before an item that sticks out is ignored.
\par 
\par \b Interface only:\b0  Turns off all of selected alignments outside of the interface section.
\par 
\par \b Unaligned\b0 : This is the number of unaligned statements allowed without ending the block of alignment. This setting was introduced in version 0.62. The default value of zero should produce the same behavior as previous versions. This setting controls how the end of a series of aligned statements is detected. When the value is zero, any unaligned statement (e.g. a blank line) ends the block of alignment, and you get output like this:
\par 
\par  
\par \f2   a   := 2        // a comment
\par   bee := 'three'; // a blank line follows
\par 
\par   cee     := 1210; // second block, separate alignment after the blank line
\par   deeFgee := 12;   // last comment
\par \f0 
\par Changing the unaligned setting to 1 means that a singe blank line does not interrupt alignment (though two blank lines would). Output would look like this:
\par 
\par  
\par \f2   a       := 2        // a comment
\par   bee     := 'three'; // a blank line follows
\par 
\par   cee     := 1210;    // same block, same alignment after the blank line
\par   deeFgee := 12;      // last comment
\par \f0 
\par How you set this is purely a matter of personal preference, as alignment of any kind is not standard. 
\par 
\par 
\par }
120
Scribble120
Clarify - Capitalisation
Clarify - Capitalisation



Writing



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Capitalisation\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par This controls capitalisation of Delphi syntax and of identifiers. Delphi syntax can be divided into Reserved words, operators, types, directives and constants. See the unit \i Wordmap.pas\i0  for the program's catalog of these words.
\par 
\par \b Any Words:\b0  A number of well known identifiers  (e.g. \b\f2 TStringList\b0 , \b AnsiCompareText\b0 , \b mbOK\b0\f0 ) defined in Delphi's standard units have their default capitalisation specified here. This list can be extended or replaced. If you have defined a type, constant or variable and want to regularise the capitalisation, enter it here.
\par 
\par \lang2057\f1 T\lang1031\f0 his setting does not apply to directives, when they actually are used as directives. 
\par 
\par Some oddities may still occur:  For e.g. In the Source code formatter source code, "Word" is both a built in type and a token property name. One capitalisation is used for both contexts. This will be disambiguated in a later release. 
\par }
130
Scribble130
Clarify - Find and replace
Clarify - Find and replace



Writing



FALSE
26
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Find and replace\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 130.\lang3079\f1 gif\lang1031\f0\}
\par 
\par \cf0\lang2057\b\f1 This feature is not yet working in JCF 2.0. It will be enabled in a later release\b0 
\par 
\par F\lang1031\f0 ind and replace is an operation that can change the semantics of your code. It is thus turned off by default. It is up to you to turn it on and to enter reasonable values. Bad configuration here can eat your code.
\par 
\par Both the GUI and the processing of find and replace is quite simple in this release. The text to be found must be a single token, and must be a word, i.e. a the name of a procedure, unit, variable, etc. Regular expressions are not supported. matching is not case sensitive.
\par 
\par Each line in the list should contain two words separated by a semicolon. These are a  source and target pair.
\par 
\par e.g. Say you have upgraded to Delphi 5 and have discovered the joys of function overloading. You wish to replace
\par \f2  \b procedure \b0 SomeOperationInteger(var pi: integer);
\par  \b procedure \b0 SomeOperationFloat(var pf: Double);
\par \f0 with
\par \f2  \b procedure \b0 SomeOperation(var pi: integer); overload;
\par  \b procedure \b0 SomeOperation(var pf: float); overload;
\par 
\par \f0 You have written the new procedures, now you have 101 units that don't compile because they still expect the old code. To fix them, enter  text as follows into the find and replace box:
\par \f2 SomeOperationInteger;SomeOperation
\par SomeOperationFloat;SomeOperation
\par }
140
Scribble140
Clarify - Find and replace on uses
Clarify - Find and replace on uses



Writing



FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fswiss Arial;}{\f1\fswiss\fcharset0 Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\strike\f0\fs20 Clarify\f1  settings\cf2\strike0\{linkID=%60\}\cf1\f0\fs32  Find and replace on uses\cf0\b0\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \cf2\{bmc 140.\lang3079\f1 gif\lang1031\f0\}\cf0 
\par 
\par \lang2057\b\f1 This feature is not working yet in V2.0. It will be enabled in a later beta.
\par \lang1031\f0 
\par \b0 This specifies operations upon uses clauses in all units processed. Check each option to turn it on. by default, all are off. unit names listed under "Remove" will be removed from any uses clause where they are found, unit names in the "insert into interface/implementation" will be inserted into all. "replace" will find occurrences or one or more of the items in  the lest-hand box and replace them will all of the items listed in the right-hand box. 
\par 
\par For e.g. you may have consolidated your constants files, and wish to replace references to units called \f2 DBConstants, LocalConst\f0  or \f2 ImageConst\f0  with one unit, \f2 AllConstants\f0 . Put  \f2 DBConstants, LocalConst\f0  and \f2 ImageConst\f0  in the left-hand box, and \f2 AllConstants\f0  in the right.
\par 
\par Note that only one find/replace can be done per run. if you wish
\par to replace \f2 UnitA\f0  with \f2 UnitB\f0 , and also replace \f2 UnitC\f0  with
\par \f2 UnitD\f0 , this will require two runs.
\par }
200
Scribble200
Command line parameters




Writing



FALSE
55
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang3079\b\fs32 Command line parameters\cf0\lang1031\b0\f1\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par This is not a DOS program, but a 32-bit, windows console application. Almost all the code is common with the GUI program. 
\par 
\par Settings specified on the command line correspond to settings described under the GUI program. All settings that have been configured vis the GUI will be used by the commandline, except for those that are overridden on the command line. Settings specified on the command line are not saved to the registry. 
\par 
\par  Here is the help from the commandline program, ie the output of "jcf -?"
\par 
\par \b Jedi Code formatter\b0 
\par  A Delphi Object-Pascal Source code formatter
\par Syntax: \f2 jcf [options] path/filename
\par \f1 Parameters to the command-line program: 
\par 
\par \b Mode of operation:\b0 
\par  \f2 -obfuscate\f1  Obfuscate mode or 
\par  \f2 -clarify\f1  Clarify mode
\par   When neither is specified, registry setting will be used. This normally means clarify.
\par 
\par \b Mode of source:\b0 
\par  -F Format a file. The file name must be specified.
\par  -D Format a directory. The directory name must be specified.
\par  -R Format a directory tree. The root directory name must be specified.
\par   When no file mode is specified, registry setting will be used.
\par 
\par \b Mode of output:\b0  
\par  \f2 -inplace\f1  change the source file without backup
\par  \f2 -out\f1  output to a new file
\par  \f2 -backup\f1  change the file and leave the original file as a backup
\par When no output mode is specified, registry setting will be used.
\par 
\par \b Other options\b0 
\par  \f2 -y\f1  No prompts to overwrite files etc. Yes is assumed
\par  \f2 -?\f1  Display this help
\par 
\par \lang3079\f0 --------------------------------------------------------------------------------------------------------------------------------\lang1031\f1 
\par 
\par Examples:
\par 
\par Format all .pas and .dpr files in the currect directory, and put the output of each to a .out file. Overwrite existing .out files.
\par 
\par \f2 jcf -clarify -out -y -d .\f1 
\par 
\par Format the file test.pas in place
\par 
\par \f2 jcf -clarify -inplace -f test.pas\f1 
\par 
\par Obfuscate all .pas and .dpr files under C:\\Code\\Delphi and leave the original as a backup
\par 
\par \f2 jcf -obfuscate -backup -r C:\\Code\\Delphi
\par 
\par \f1 
\par }
210
Scribble210
Special comments




Writing



FALSE
36
{\rtf1\ansi\ansicpg1252\deff0\deflang1031{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil\fcharset0 Courier New;}{\f3\fnil Courier New;}{\f4\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang3079\b\fs32 Special comments\cf0\lang1031\b0\f1\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par The code formatter now recognizes certain comments to disable and enable formatting options around blocks of code.
\par 
\par Since the earliest versions, the comments \f2\{(*\}\f1  and \f2\{*)\}\f1  could be used to disable and reenable formatting respectively, in order to exclude a block of code from formatting. 
\par 
\par From version 0.52, a more extensive syntax has been implemented to allow for fine-grained control over which formatting options are temporarily disabled. The original special comments\f2  \{(*\}\f1  and \f2\{*)\}\f1  are retained as synonymns for \f2 //jcf:format=off\f1  and \f2 //jcf:format=on\f1   respectively.
\par 
\par For instance, in the following line of code, the formatter will not alter the spacing
\par 
\par \f2 //jcf:space=off
\par  a  :=   a   +   1  ;
\par //jcf:space=on
\par \f1 
\par As you can see, the new syntax has the form of a comment like \f2 //jcf:\b flag\b0 =\b state\b0\f1 , where \b state \b0 is one of \b on \b0 or \b off\lang3079\f0 .\b0 
\par 
\par Applicable flags are 
\par \f2 all, format, obfuscate, space, addspace, removespace, return, addreturn, removereturn, add, remove, align, aligndef, alignfn, alignvars, alignconst, aligntypedef, alignassign, indent, caps, capsreservedwords, capsspecificwords, linebreaking, blockstyle, warnings, findreplace, findreplaceuses
\par \f0 
\par A table with a description of all \lang1031\f1 flags \lang3079\f0 can be found at \cf1\strike\f3 http://jedicodeformat.sourceforge.net/comments.html\cf2\strike0\{link=*! ExecFile("http://jedicodeformat.sourceforge.net/comments.html")\}\f2 .\cf0\f0 
\par 
\par \lang1031\f1 The following rules apply:
\par 
\par \pard{\pntext\f4\'B7\tab}{\*\pn\pnlvlblt\pnf4\pnindent0{\pntxtb\'B7}}\fi-200\li200 Turning a flag on will not enable it if it is not disabled in the configuration - the flags are to 'block out' a section of code where a format option is not wanted. The on state is there only to end a previous off state.
\par {\pntext\f4\'B7\tab}All processes turned off buy a flag comment remain turned off until the corresponding flag comment that turns it back on is reached, or the end of the file is reached, or the flag comment \f2 //jcf:all=on\f1  is reached.
\par {\pntext\f4\'B7\tab}The flag comment \f2 //jcf:all=on\f1  will reenable all processes that have been turned off by comments, ie it returns formatting to the options specified in the configuration.
\par {\pntext\f4\'B7\tab}Flag comments only affect the file in which they occur, from the position in which the occur onwards.
\par {\pntext\f4\'B7\tab}The effects of some of the flags overlap - this is done to allow degrees of granularity e.g. turn off all alignment, or just alignment of variable declarations for a block of code.
\par {\pntext\f4\'B7\tab}In these special comments, character case and some spaces are ignored.
\par {\pntext\f4\'B7\tab}A comment that starts with \f2 //jcf:\f1  but cannot be recognised as one of the options given will cause a warning in the log but will have no effect of the file's format.
\par {\pntext\f4\'B7\tab}\pard 
\par For exampes see the test case unit \f2 TestExclusionFlags.pas\f1 , for impl\lang2057\f0 e\lang1031\f1 mentation see the unit \f2 FormatFlags.pas\f1 
\par }
220
Scribble220
Issues




Writing



FALSE
44
{\rtf1\ansi\ansicpg1252\deff0\deflang2057{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\fs32 Issues\cf2\b0\f1\fs20 
\par 
\par \f0 All new issues should be reported via the SourceForge bug tracking system for this project.\f1 
\par 
\par \f0 the main known issue is that the Jedi Code Formatter does not play well with compiler directives. It ignores tham completely. Thus your code must have sensible if the comiler directives were ignored.
\par 
\par e.g.
\par \f2 
\par \b procedure \b0 BadCompilerDirective;
\par \b begin\b0 
\par \{$IFDEF FOO\}
\par 
\par    DoSomething();
\par  
\par  
\par \b end\b0 ; 
\par   \{$ELSE\}
\par 
\par    DoSomethingElse();
\par \b 
\par end\b0 ;\f1 
\par \f2  \{$ENDIF\}
\par 
\par \f0 This cannot be formatted, as without the compiler directives it has too many \b\f2 end\f0  \b0 statements.\f2 
\par 
\par \b procedure \b0 GoodCompilerDirective;
\par \b begin\b0 
\par   \{$IFDEF FOO\}
\par 
\par    DoSomething();
\par  
\par   \{$ELSE\}
\par 
\par    DoSomethingElse();
\par 
\par  \{$ENDIF\}
\par \b end\b0 ;
\par 
\par \f0 This cannot be formatted, as the block structure is not interlaced with the comiler directives.\f2 
\par 
\par \b 
\par }
1000
Scribble1000
Info...
Info



Writing



FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang3079{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss Arial;}{\f3\fmodern Courier New;}{\f4\fswiss\fcharset0 Arial;}{\f5\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\lang1031\b\fs32 Info...\cf0\lang3079\b0\f1\fs20 
\par \cf2\{keepn\}\cf0 
\par 
\par \lang1031\f2 This documantation was made out of the web page provided by Anthony Steele on his webpage on
\par \cf1\strike\f3 http://jedicodeformat.sourceforge.net/\cf2\strike0\{link=*! ExecFile("\cf1\strike http://jedicodeformat.sourceforge.net/\cf2\strike0 ")\}\cf0\f2 
\par 
\par I really love the tool but I sometimes have to play around with the options to find out what they do. 
\par That's why I put together this help file...
\par 
\par Feel free to send additions/corrections, better explanations or whatever concerning this helpfile to \cf1\strike\f3 codeformat@spoonworx.com\cf2\strike0\{link=*! ExecFile("mailto:codeformat@spoonworx.com")\}\cf0\f2 . \lang3079\f4 You can also get the source for this helpfile from the Sourceforge CVS. The helpfile is created using HelpScribble, and you can use the free demo-version (available from the Helpscribble-homepage from \cf1\strike\f5 http://www.helpscribble.com\cf2\strike0\{link=*! ExecFile("http://www.helpscribble.com")\}\cf0\f4 ) to make changes to this helpfile. If you will send us your modified helpfile (the source), we will try to include your changes into the next possible release.
\par \lang1031\f2 
\par \f4 C\f2 omments regarding the program should be sent to Anthony Steele (\cf1\strike\f3 anthonysteele@users.sourceforge.net\cf2\strike0\{link=*! ExecFile("anthonysteele@users.sourceforge.net")\}\cf0\f2 ).
\par \lang3079\f1 
\par \lang1031\f0 Borland styleguides can be found at
\par \cf1\strike\f5 http://community.borland.com/article/0,1410,10280,00.html\cf2\strike0\{link=*! ExecFile("http://community.borland.com/article/0,1410,10280,00.html")\}\cf0\f0 
\par 
\par Ralf Steinhaeusser,
\par SpoonworX Inc.\lang3079\f1 
\par }
1
main="",(120,44,658,726),0,(255,255,255),(255,255,196),0
0
0
0
6
*InternetLink
16711680
Courier New
0
10
1
....
0
0
0
0
0
0
*ParagraphTitle
-2147483640
Arial
0
11
1
B...
0
0
0
0
0
0
*PopupLink
-2147483640
Arial
0
8
1
....
0
0
0
0
0
0
*PopupTopicTitle
16711680
Arial
0
10
1
B...
0
0
0
0
0
0
*TopicText
-2147483640
Arial
0
10
1
....
0
0
0
0
0
0
*TopicTitle
16711680
Arial
0
16
1
B...
0
0
0
0
0
0
