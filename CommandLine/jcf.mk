# Author - Denis Golovan aka MageSlayer
# developed for Jedi Code Format (http://jedicodeformat.sourceforge.net/)
# Contact emails - denis.golovan@gmail.com, denisgolovan@yandex.ru
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
# USA.

#build options file
#check all paths in order to build JCF successfully

#these should be in relative form
JCF_ROOT=../.

#uncomment if needed
BUILD_PLATFORM=LINUX
#BUILD_PLATFORM=WIN32

ifeq ($(BUILD_PLATFORM),LINUX)
  # load configure detected options
  include fpc.mk
endif

PLATFORM=LINUX
LAZARUS_ROOT=$(lazarus_root)
FPC_ROOT=$(fpc_root)
INSTALL_DIR=$(bindir)

#uncomment if needed and define proper path to delphi root directory in DELPHI_ROOT
#PLATFORM=WIN32
#WINE=1  #for using Wine emulator on *nix systems (crosscompiling, sort of)
#DELPHI=1
#should be in absolute form. Symlink tinkering...
DELPHI_ROOT="/home/denis/.wine/drive_d/Delphi7SE"

PROJECT_FILE := $(JCF_ROOT)/CommandLine/Lazarus/JCF.lpr
OBJ_DIR_ROOT := $(JCF_ROOT)/Output
obj_dir := $(OBJ_DIR_ROOT)/Lazarus
EXE_DIR := $(OBJ_DIR_ROOT)/Lazarus

#conditional define for build-time feature selection
COND_DEFINES=COMMAND_LINE 

# some checks
ifeq ($(BUILD_PLATFORM),WIN32)
  ifeq ($(PLATFORM),WIN32)
    $(error Building using Delphi on Win32 is not supported. Only Linux Wine cross-compilation is implemented. \
Patches are welcome)
  endif
endif

ifneq ($(BUILD_PLATFORM),WIN32)
  ifneq ($(BUILD_PLATFORM),LINUX)
    $(error Unsupported build platform - $(BUILD_PLATFORM))
  endif
endif

ifneq ($(PLATFORM),WIN32)
  ifneq ($(PLATFORM),LINUX)
    $(error Unsupported platform to build for - $(PLATFORM))
  endif
endif

#platform specific options
ifeq ($(PLATFORM),WIN32)
  #Win32 options
  ifdef DELPHI
    PROJECT_FILE := $(JCF_ROOT)/CommandLine/D11/JCF.dpr
    obj_dir := $(OBJ_DIR_ROOT)/D7
    EXE_DIR := $(OBJ_DIR_ROOT)/D7
    DCC := dcc32.exe 
    ifdef WINE
      WINE_PREFIX:=WINEPREFIX="$(realpath $(JCF_ROOT)/..)"
      WINE_SHELL:=$(WINE_PREFIX) wineconsole --backend=curses 
      SHELL_SCRIPT:=dcc_compile.bat
    endif
  endif

  PROJECT_BIN:=jcf.exe
else
  #Linux options
  FPC := $(fpcc)

  COND_DEFINES += UNIX

  PROJECT_BIN:=jcf
endif


ifdef RELEASE
  FPC_OPTS :=-n -Un -XX -O2 -Xs
  DCC_OPTS :=-O+ -I- -W- -Q- -R-
else
  FPC_OPTS :=-n -Un -vb -CX -XX -gl -WN -vewnhi -l  
  DCC_OPTS :=-O- -I+ -W+ -Q+ -R+
endif
