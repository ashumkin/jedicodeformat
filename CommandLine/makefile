include jcf.mk

#TODO
#+make make build option (-B) pass to pascal compiler
#+implement clean target
#+implement proper Windows path transform
#+add BUILD_PLATFORM - platform on which we build physically
#+add checks for validity of BUILD_PLATFORM, PLATFORM
#+add check for win32 native delphi build (impossible for now)
#+added installing under unix

FPC_OPTS_FILE :=$(obj_dir)/fpc.cfg

#=============================================================================

inc_dirs += $(JCF_ROOT)/Include

ifndef DELPHI
  LCL_ROOT:=$(LAZARUS_ROOT)/lcl
  lib_dirs += $(LCL_ROOT)
  lib_dirs += $(LCL_ROOT)/include
  lib_dirs += $(LCL_ROOT)/widgetset
  lib_dirs += $(LCL_ROOT)/interfaces/nogui

  inc_dirs += $(LCL_ROOT)/include

  lib_dirs += $(FPC_ROOT)/source/packages/fcl-base/src
  lib_dirs += $(FPC_ROOT)/source/packages/fcl-image/src
  lib_dirs += $(FPC_ROOT)/source/packages/paszlib/src
  lib_dirs += $(FPC_ROOT)/source/packages/fcl-registry/src
  lib_dirs += $(FPC_ROOT)/source/packages/fcl-xml/src
else
  DELPHI_DIR:=$(notdir "$(DELPHI_ROOT)")
  DELPHI_SANDBOX_ROOT=c:\\$(DELPHI_DIR)

  lib_bin_dirs += "$(DELPHI_SANDBOX_ROOT)\\Lib"
endif

#Delphi does not support directory wildcards. Grr...
src_dirs += $(JCF_ROOT)
src_dirs += $(JCF_ROOT)/CommandLine
src_dirs += $(JCF_ROOT)/JcfGui
src_dirs += $(JCF_ROOT)/Process
src_dirs += $(JCF_ROOT)/ReadWrite
src_dirs += $(JCF_ROOT)/Settings
src_dirs += $(JCF_ROOT)/Ui
src_dirs += $(JCF_ROOT)/Utils
src_dirs += $(JCF_ROOT)/Parse/
src_dirs += $(JCF_ROOT)/Parse/PreProcessor
src_dirs += $(JCF_ROOT)/Parse/UI
src_dirs += $(JCF_ROOT)/Process/
src_dirs += $(JCF_ROOT)/Process/Align
src_dirs += $(JCF_ROOT)/Process/Capitalisation
src_dirs += $(JCF_ROOT)/Process/Indent
src_dirs += $(JCF_ROOT)/Process/Info
src_dirs += $(JCF_ROOT)/Process/Obfuscate
src_dirs += $(JCF_ROOT)/Process/OnceOffs
src_dirs += $(JCF_ROOT)/Process/Returns
src_dirs += $(JCF_ROOT)/Process/Spacing
src_dirs += $(JCF_ROOT)/Process/Transform
src_dirs += $(JCF_ROOT)/Process/Warnings
src_dirs += $(JCF_ROOT)/Settings/Streams
#=============================================================================
#http://www.cmcrossroads.com/content/view/6504/120/
# input_int consists of 65536 x's built from the 16 x's in 16

16 := x x x x x x x x x x x x x x x
input_int := $(foreach a,$(16),$(foreach b,$(16),$(foreach c,$(16),$(16)))))

# decode turns a number in x's representation into a integer for human
# consumption

decode = $(words $1)

# encode takes an integer and returns the appropriate x's
# representation of the number by chopping $1 x's from the start of
# input_int

encode = $(wordlist 1,$1,$(input_int))

# increment adds 1 to its argument, decrement subtracts 1.  Note that
# decrement does not range check and hence will not underflow, but
# will incorrectly say that 0 - 1 = 0

increment = $1 x
decrement = $(wordlist 2,$(words $1),$1)

#dirty hack to filter spaces between dirs
nullstring := 
space := $(nullstring) # end of the line

# returns last word in list
last-word=$(word $(words $(1)), $(1))

# decrements number. Who said GNU make does not have arithmetic?
prev-num=$(call decode, $(call decrement, $(call encode, $(1))))

#correct semi-colon add. Last word does not have ";" appended
#ex. $(call semicolon-add,$(src_dirs))
semicolon-add=$(addsuffix ;,$(wordlist 1, $(call prev-num,$(words $(1))), $(1))) $(call last-word, $(1))

# filter unneeded current directory markers
filter-dot-dir=$(subst /./,/,$(1))

# win-dirs-list - function converts a unix-style make path list (separated with spaces)
# to Windows platform and flattens to ;-separated list
# if it has second param, it append in front of each path
#
# ex. echo "$(call win-dirs-list, $(src_dirs),..\\)" >> $(WINE_SANDBOX_SCRIPT)
win-dirs-list=$(subst $(space),, $(subst /,\\, $(call filter-dot-dir, $(addprefix $(2), $(call semicolon-add, $(strip $(1)))))))
#=============================================================================
#check all project files
#WARNING!!! only JCF source files are checked for modification
pas_files := $(addsuffix /*.pas,$(src_dirs))
pas_files := $(wildcard $(pas_files))
inc_files := $(addsuffix /*.inc,$(inc_dirs))
inc_files := $(wildcard $(inc_files))

src_dirs := $(lib_dirs) $(src_dirs) 
exe_bin := $(EXE_DIR)/$(PROJECT_BIN)

# should we do "build" instead of "make"?
ifneq (,$(findstring B,$(MAKEFLAGS)))
  FPC_OPTS += -B
  DCC_OPTS += -B
endif

default : all

all : $(exe_bin)
.PHONY : clean

# cleaning all object files. Hidden directories are excluded
ifeq ($(BUILD_PLATFORM),LINUX)
clean : 
	find $(call filter-dot-dir, $(OBJ_DIR_ROOT)) -type f \( ! -regex '.*/\..*' \) -exec $(RM) '{}' \;
endif

ifeq ($(BUILD_PLATFORM),WIN32)
clean : 
	$(error Cleaning on Win32 is not implemented)
endif

ifeq ($(PLATFORM),WIN32)
  ifdef DELPHI
    # seems like Borland compiler likes dirs relative to project file
    # and not relative to the current directory... Grrr...
    src_dirs_for_DCC:=$(call win-dirs-list, $(src_dirs),../)
    inc_dirs_for_DCC:=$(call win-dirs-list, $(inc_dirs),../)
    EXE_DIR_for_DCC:=$(call win-dirs-list, $(EXE_DIR),../)

    DCC_OPTS += $(addprefix -D,$(COND_DEFINES))

    DCC_WIN32 := $(DELPHI_SANDBOX_ROOT)\\bin\\$(DCC) $(DCC_OPTS) -I$(inc_dirs_for_DCC) -U$(src_dirs_for_DCC) -U$(lib_bin_dirs) -N$(EXE_DIR_for_DCC) -E$(EXE_DIR_for_DCC)

#rules
WINE_SANDBOX_ROOT=$(JCF_ROOT)/..
WINE_SANDBOX=$(WINE_SANDBOX_ROOT)/drive_c
WINE_SANDBOX_SCRIPT=$(WINE_SANDBOX)/$(SHELL_SCRIPT)

$(exe_bin) : $(pas_files) $(inc_files) $(PROJECT_FILE)
	$(WINE_PREFIX) wineprefixcreate -w &> /dev/null
	ln -sf $(DELPHI_ROOT) $(WINE_SANDBOX)
	ln -sf $(realpath $(JCF_ROOT)) $(WINE_SANDBOX)
	echo "c:" > $(WINE_SANDBOX_SCRIPT)	
	echo "cd Jcf2\\CommandLine\\D11" >> $(WINE_SANDBOX_SCRIPT)
	echo "$(DCC_WIN32) $(notdir $(PROJECT_FILE)) > ..\\make.log" >> $(WINE_SANDBOX_SCRIPT)
	$(WINE_SHELL) $(WINE_SANDBOX_SCRIPT)

install : $(exe_bin)
	echo "Installing under Windows is not supported. Find your file - " $(exe_bin)
  endif
endif

ifndef DELPHI
  src_dirs_for_FPC := $(addsuffix \n,$(src_dirs))
  src_dirs_for_FPC := $(addprefix -Fu,$(src_dirs_for_FPC))  
  
  inc_dirs_for_FPC := $(addsuffix \n,$(inc_dirs))
  inc_dirs_for_FPC := $(addprefix -Fi,$(inc_dirs_for_FPC))

  FPC_COND_DEFINES := $(addprefix -d,$(COND_DEFINES))
  FPC_COND_DEFINES := $(addsuffix \n,$(FPC_COND_DEFINES))

  FPC_OPTS := $(addsuffix \n,$(FPC_OPTS))
  FPC_OPTS += -FU$(obj_dir)\n
  
  FPC_OPTS += $(src_dirs_for_FPC)
  FPC_OPTS += $(inc_dirs_for_FPC)

  FPC_OPTS += $(FPC_COND_DEFINES)
  
  FPC_OPTS += -o$(exe_bin)
#rules

$(exe_bin) : $(pas_files) $(inc_files) $(PROJECT_FILE) 
	echo -e "$(FPC_OPTS)" > "$(FPC_OPTS_FILE)"	
	$(FPC) @$(FPC_OPTS_FILE) $(PROJECT_FILE)

install : $(exe_bin)
	$(INSTALL_EXE) $(exe_bin) $(INSTALL_DIR)
endif

