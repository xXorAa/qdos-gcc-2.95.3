/* Definitions of target machine for GNU compiler.  QDOS version.
   Copyright (C) 2000 Richard Zidlicky rz@linux-m68k.org

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*
  Definition for gcc-qdos target. Compiles using c68 calling conventions
  and as68 assembler output
 */

#undef USE_GAS

/* QDOS: use QDOS-c68 rules for ANSI arg promotion rules, this turns off
   PROMOTE_PROTOTYPES in m68k.h. See also PARM_BOUNDARY
   QDOS also used to control a few things in libgcc.a
*/

#ifndef QDOS
#define QDOS
#endif


#ifndef USE_GAS
/* This controls conditionals in m68k.h.  */
#define MOTOROLA		/* Use Motorola syntax rather than "MIT" */
#define MOTOROLA_BSR
#define AS68         /* as68 needs displ(reg) instead of (displ,reg) */
#define SGS_SWITCH_TABLES
#define SGS_NO_LI
#endif

#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

#include "m68k/m68k.h"

#undef SELECT_RTX_SECTION  /* m68k.h overrides the default which isn't good */

/* needed for c68 calling conventions */
#undef PARM_BOUNDARY
#define PARM_BOUNDARY 16

/* special c68 support library calling conventions */
#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)  qdos_return_pops_args(FUNDECL,FUNTYPE,SIZE)

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME) qdos_asm_output_labelref(FILE,NAME)

#if 0
/* Beginning of c68 PIC support, f-prologue,epilogue and reload1.c must be fixed */
#undef PIC_OFFSET_TABLE_REGNUM
#define PIC_OFFSET_TABLE_REGNUM  14 /* c68 uses a6 ?*/
/*#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED*/ /* not clobbered by calls in c68 */

#undef CONDITIONAL_REGISTER_USAGE
#define CONDITIONAL_REGISTER_USAGE \
{ 						\
  if (flag_pic)					\
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]		\
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;\
}

/* Base register for access to local variables of the function.  */
#undef FRAME_POINTER_REGNUM
#define FRAME_POINTER_REGNUM (flag_pic ? 13 : 14)

/* Base register for access to arguments of the function.  */
#undef ARG_POINTER_REGNUM
#define ARG_POINTER_REGNUM (flag_pic ? 13 : 14)
#endif /* C68 PIC support */

/* some other bits that may be needed to make it c68 compatible
   see mot3300.h for library call hacks
*/
#if 0
/* This will return small structs in d0.  */
#define RETURN_IN_MEMORY(type) \
  ((TYPE_MODE (type) == BLKmode) \
   || (AGGREGATE_TYPE_P (type) \
       && GET_MODE_SIZE (TYPE_MODE (type)) > UNITS_PER_WORD))

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
#endif


#define TARGET_DEFAULT 0 /*(MASK_BITFIELD|MASK_68881|MASK_68020)*/

/* use fomit-frame-pointer as default for -O>=2 or -Os*/
#define OPTIMIZATION_OPTIONS(OPTIMIZE,SIZE) \
 {  								\
   if (OPTIMIZE >= 2 || (SIZE)) 						\
     flag_omit_frame_pointer = 1;				\
 }

#define MASK_C68MATH  8192
#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
        {"c68lib", +MASK_C68MATH, "use c68 version of math support library"},

#define C68LIB (target_flags & MASK_C68MATH)

/* QDOS sections:
 text
 data   initialised data
 udata  uninitialised data
 RLSI   linker relocation
 bss    initialisation data
*/

#define IS_GWASS  (target_flags & (MASK_68020 | MASK_68881))

#undef DATA_SECTION_ASM_OP
#undef TEXT_SECTION_ASM_OP

#define DATA_SECTION_ASM_OP (IS_GWASS ? "    SECTION data" : "\t.sect\t.data" )
#define TEXT_SECTION_ASM_OP (IS_GWASS ? "    SECTION text" :"\t.sect\t.text")
#define BSS_SECTION_ASM_OP (IS_GWASS ? "    SECTION bss" :"\t.sect\t.bss")             /* change to udata ?*/


#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE,NAME,SIZE,ROUNDED) qdos_asm_output_common(FILE,NAME,SIZE,ROUNDED)

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE,NAME,SIZE,ROUNDED) qdos_asm_output_local(FILE,NAME,SIZE,ROUNDED)


#define ASM_OUTPUT_BSS(FILE,DECL,NAME,SIZE,ROUNDED) \
(  asm_output_bss(FILE,DECL,NAME,SIZE,ROUNDED),\
   fputs("\t.align\t2\n",(FILE)))

#define SPACE_ASM_OP ".space"

#define ASM_DECLARE_OBJECT(FILE,NAME,OBJECT) \
( fputs (IS_GWASS ? "    XDEF ":"\t.extern\t", (FILE)),                           \
  assemble_name ((FILE), (NAME)), fputs("\n",(FILE)))

#undef GLOBAL_ASM_OP
#ifndef IN_LIBGCC2
#define GLOBAL_ASM_OP (IS_GWASS ? "    XDEF " : "\t.extern\t")
#else
#define GLOBAL_ASM_OP "\t.extern\t"
#endif

/* Define __HAVE_68881__ in preprocessor, unless -msoft-float is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#ifdef _WIN32
#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__}  %{.S:-P} %{.asm:-P} -I/qdos-gcc/include -I/qdos-gcc/include/sys"
#else
#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__}  %{.S:-P} %{.asm:-P} -I/usr/local/qdos-gcc/include -I/usr/local/qdos-gcc/include/sys"
#endif

/* ignore -g or -p flags */
#define CC1_SPEC " %{g:} %{p:}"

/* no -g or -p option  */

#ifdef _WIN32
#define LINK_SPEC " -L/qdos-gcc/lib "
#else
#define LINK_SPEC " -L/usr/local/qdos-gcc/lib "
#endif

#define LIB_SPEC " -lc "

#ifdef _WIN32
#define LINKER_NAME "/qdos-gcc/bin/ld "
#else
#define LINKER_NAME "/usr/local/qdos-gcc/bin/ld "
#endif

#define STARTFILE_SPEC ""   /* suppress crt0.o (c68 ld does it itself) */
#define LIBGCC_SPEC "-lgcc" /* (don't) suppress -lgcc */

/* define gwass flag for anything that won't assemble with as68 */
#define ASM_SPEC "%{m68020:-gwass}%{m68030:-gwass} %{m68040:-gwass} %{m68881:-gwass} %{m68060:-gwass} %{m68020-40:-gwass} %{m68020-60:-gwass} %{m68020-60:-gwass} %{m68040-60:-gwass}"

#ifdef _WIN32
#define TOOLDIR_BASE_PREFIX  "/qdos-gcc/"
#endif
/*#define TOOL_INCLUDE_DIR  "/usr/local/qdos-gcc/include"*/
/*#define LINK_COMMAND_SPEC "/usr/local/qdos-gcc/bin/ld "*/

/*#define STANDARD_EXEC_PREFIX "/usr/local/qdos-gcc/bin"*/
/*#define MD_STARTFILE_PREFIX "/usr/local/qdos-gcc/bin"*/
/*#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "*/

#if 0
/* this is necessary because c68-ld expects '-o outfile' in a special position */
#define LINK_COMMAND_SPEC "\
%{!fsyntax-only: \
 %{!c:%{!M:%{!MM:%{!E:%{!S:%(linker) %l %X  %{A} %{d} %{e*} %{m} %{N} %{n} \
			%{r} %{s} %{t} %{u*} %{x} %{z} %{Z}\
			%{!A:%{!nostdlib:%{!nostartfiles:%S}}}\
			%{static:} %{L*} %D %o %{o*} \
			%{!nostdlib:%{!nodefaultlibs:%G %L %G}}\
			%{!A:%{!nostdlib:%{!nostartfiles:%E}}}\
			%{T*} \
			\n }}}}}}"
#endif

/* Names to predefine in the preprocessor for this target machine.  */

#ifdef MOTOROLA
#define CPP_PREDEFINES " -Dmc68000 -DMC68000 -D__QDOS__ -D__MC68000__ -DQDOS -Asystem(qdos-gcc) -Asystem(QDOS) -Acpu(m68k) -Amachine(m68k)"
#else
#define CPP_PREDEFINES " -Dmc68000 -DMC68000 -D__QDOS__ -D__MC68000__ -DQDOS -Asystem(qdos-gcc) -Asystem(QDOS) -Acpu(m68k) -Amachine(m68k)"
#endif


/* define c68 routines to use:
   .Xulmul
   .Xuldiv
   .Xlmul
   .Xldiv

`MULSI3_LIBCALL'
     A C string constant giving the name of the function to call for
     multiplication of one signed full-word by another.  If you do not
     define this macro, the default name is used, which is `__mulsi3',
     a function defined in `libgcc.a'.

`DIVSI3_LIBCALL'
     A C string constant giving the name of the function to call for
     division of one signed full-word by another.  If you do not define
     this macro, the default name is used, which is `__divsi3', a
     function defined in `libgcc.a'.

`UDIVSI3_LIBCALL'
     A C string constant giving the name of the function to call for
     division of one unsigned full-word by another.  If you do not
     define this macro, the default name is used, which is `__udivsi3',
     a function defined in `libgcc.a'.

`MODSI3_LIBCALL'
     A C string constant giving the name of the function to call for the
     remainder in division of one signed full-word by another.  If you
     do not define this macro, the default name is used, which is
     `__modsi3', a function defined in `libgcc.a'.

`UMODSI3_LIBCALL'
     A C string constant giving the name of the function to call for the
     remainder in division of one unsigned full-word by another.  If
     you do not define this macro, the default name is used, which is
     `__umodsi3', a function defined in `libgcc.a'.

`MULDI3_LIBCALL'
     A C string constant giving the name of the function to call for
     multiplication of one signed double-word by another.  If you do not
     define this macro, the default name is used, which is `__muldi3',
     a function defined in `libgcc.a'.

`DIVDI3_LIBCALL'
     A C string constant giving the name of the function to call for
     division of one signed double-word by another.  If you do not
     define this macro, the default name is used, which is `__divdi3', a
     function defined in `libgcc.a'.

`UDIVDI3_LIBCALL'
     A C string constant giving the name of the function to call for
     division of one unsigned full-word by another.  If you do not
     define this macro, the default name is used, which is `__udivdi3',
     a function defined in `libgcc.a'.

`MODDI3_LIBCALL'
     A C string constant giving the name of the function to call for the
     remainder in division of one signed double-word by another.  If
     you do not define this macro, the default name is used, which is
     `__moddi3', a function defined in `libgcc.a'.

`UMODDI3_LIBCALL'
     A C string constant giving the name of the function to call for the
     remainder in division of one unsigned full-word by another.  If
     you do not define this macro, the default name is used, which is
     `__umoddi3', a function defined in `libgcc.a'.
*/

/* define this to use c68 support routines for math */
/* must observe special calling conventions */

#define C68_INTMATH
#define C68_FLOATMATH

#ifdef C68_INTMATH
#define MULSI3_LIBCALL (C68LIB ? "*.Xlmul" : "*.GXlmul")
#define UMULSI3_LIBCALL (C68LIB ? "*.Xulmul" : "*.GXulmul")
#define DIVSI3_LIBCALL (C68LIB ? "*.Xldiv" : "*.GXldiv")
#define UDIVSI3_LIBCALL (C68LIB ? "*.Xuldiv" : "*.GXuldiv")
#define MODSI3_LIBCALL (C68LIB ? "*.Xlrem" : "*.GXlrem")
#define UMODSI3_LIBCALL (C68LIB ? "*.Xulrem" : "*.GXulrem")
#endif

#ifdef C68_FLOATMATH
#define MULDD3_LIBCALL (C68LIB ? "*.Ydfmul" : "*.GYdfmul")
#define MULDF3_LIBCALL (C68LIB ? "*.Ysfmul" : "*.GYsfmul")
#define DIVDD3_LIBCALL (C68LIB ? "*.Ydfdiv" : "*.GYdfdiv")
#define DIVDF3_LIBCALL (C68LIB ? "*.Ysfdiv" : "*.GYsfdiv")
#define ADDD3_LIBCALL (C68LIB ? "*.Ydfadd" : "*.GYdfadd")
#define ADDF3_LIBCALL (C68LIB ? "*.Ysfadd" : "*.GYsfadd")
#define SUBDD3_LIBCALL (C68LIB ? "*.Ydfsub" : "*.GYdfsub")
#define SUBDF3_LIBCALL (C68LIB ? "*.Ysfsub" : "*.GYsfsub")

#define EXTENDSFDF2_LIBCALL (C68LIB ? "*.Ysftodf" : "*.GYsftodf") /* float -> double */
#define TRUNCDFSF2_LIBCALL (C68LIB ? "*.Ydftosf" : "*.GYdftosf")  /* double -> float */
#define FLOATSIDF_LIBCALL (C68LIB ? "*.Ydfltodf" : "*.GYdfltodf")
#define FLOATSISF_LIBCALL (C68LIB ? "*.Ysfltosf" : "*.GYsfltosf")

#define FIXDFSI_LIBCALL (C68LIB ? "*.Ydftol" : "*.GYdftol")
#define FIXSFSI_LIBCALL (C68LIB ? "*.Ysftol" : "*.GYsftol")

/* float/double compare have even more special calling
   conventions */
#if 0
#define CMPDD3_LIBCALL "*.Xdfcmp"
#define CMPDF3_LIBCALL "*.Xsfcmp"
#endif
#endif /* C68_FLOATMATH */

/* useless for now  */
#define DBX_DEBUGGING_INFO
#define ASM_STABS_OP "; .stabs"   /* output as comment */
#define ASM_STABD_OP "; .stabd"
#define ASM_STABN_OP "; .stabn"


#undef CALL_USED_REGISTERS
#undef FUNCTION_PROFILER

#ifdef MOTOROLA
#if 0
#undef FUNCTION_PROLOGUE
#undef FUNCTION_EPILOGUE
#endif
#undef REGISTER_NAMES
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP
#undef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_SKIP
#undef ASM_FORMAT_PRIVATE_NAME
#endif

#undef ASM_OUTPUT_ALIGN

/* There is no point aligning anything to a rounder boundary than this.  */
/*#define BIGGEST_ALIGNMENT 16*/
#define BIGGEST_FIELD_ALIGNMENT 16

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 0
/* size of struct multiple of * bits */
#define STRUCTURE_SIZE_BOUNDARY 16
#define BITFIELD_NBYTES_LIMITED 1
#undef EMPTY_FIELD_BOUNDARY
#define EMPTY_FIELD_BOUNDARY 16

/* for ADJUST_FIELD_ALIGN  see rs6000/rs6000.h */

/* QDOS makes d0-d2, a0-a1, fp0-fp2 unsaved registers  */
/* aehm, just guessing the fp regs for now */

#define CALL_USED_REGISTERS \
 {1, 1, 1, 0, 0, 0, 0, 0, \
  1, 1, 0, 0, 0, 0, 0, 1, \
  1, 1, 1, 0, 0, 0, 0, 0}


/* QDOS returns ints, pointers floats and doubles in  d0/d1.  */
#if 0
#define FUNCTION_VALUE(VALTYPE,FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))

#define LIBCALL_VALUE(MODE)						   \
 gen_rtx (REG, (MODE),							   \
	  ((TARGET_68881						   \
	    && ((MODE) == SFmode || (MODE) == DFmode || (MODE) == XFmode)) \
	   ? 16 : 0))
#endif

/* no info about struct return in C68 */
#undef PCC_STATIC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

#define ASM_OUTPUT_ALIGN(FILE,LOG) qdos_asm_output_align(FILE,LOG)


#undef FINALIZE_TRAMPOLINE
#define FINALIZE_TRAMPOLINE(TRAMP)					\
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "_CacheFlush"),	        \
		     0, VOIDmode, 2, TRAMP, Pmode,			\
		     plus_constant (TRAMP, TRAMPOLINE_SIZE), Pmode);


/* as68 conventions */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE) qdos_asm_start_file(FILE)


/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#undef ASM_APP_ON
#define ASM_APP_ON "; handmade assembler code follows\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#undef ASM_APP_OFF
#define ASM_APP_OFF "; gcc generated assembler follows\n"

#ifdef MOTOROLA

/* Don't try to define `gcc_compiled.' since the assembler does not
   accept symbols with periods.  This is no real loss since GDB only
   really needs it for parms passed in registers.  */
#define ASM_IDENTIFY_GCC(FILE)  fputs(";gcc2_compiled\n", FILE)

#undef ASM_OUTPUT_INT
#undef ASM_OUTPUT_LONG
#undef ASM_OUTPUT_SHORT
#undef ASM_OUTPUT_CHAR
#undef ASM_OUTPUT_BYTE
#undef ASM_BYTE_OP

#define ASM_LONG    "\t.data4"
#define LONG_ASM_OP "\t.data4"


#define ASM_BYTE_OP	"\t.data1\t"
#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf ((FILE), IS_GWASS ? "\tDC.L ":"\t.data4 "),			\
  output_addr_const ((FILE), (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_LONG(FILE,VALUE)  \
( fprintf ((FILE), IS_GWASS ? "\tDC.L ":"\t.data4 "),			\
  output_addr_const ((FILE), (VALUE)),		\
  fprintf ((FILE), "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf ((FILE), IS_GWASS ? "\tDC.W ":"\t.data2 "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf ((FILE), IS_GWASS ? "\tDC.B ":"\t.data1 "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  asm_fprintf (FILE, IS_GWASS ? "\tDC.W %LL%d-%LL%d\n" : "\t.data2 %LL%d-%LL%d\n", VALUE, REL)

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, IS_GWASS ? "\tDC.B $%X\n":"\t.data1 0x%x\n", (VALUE))



/* Translate Motorola opcodes such as `jbeq'
   into opcodes such as `beq'.
*/

#undef ASM_OUTPUT_OPCODE
#define ASM_OUTPUT_OPCODE(FILE, PTR)			\
{ if ((PTR)[0] == 'j' && (PTR)[1] == 'b')		\
    { ++(PTR);						\
      while (*(PTR) != ' ')				\
	{ putc (*(PTR), (FILE)); ++(PTR); }		\
    }                                                   \
  else if ((PTR)[0] == 'j' && (PTR)[1] == 'c' && (PTR)[2] == 'c') \
    {putc('b',(FILE));putc('c',(FILE));putc('c',(FILE));(PTR)+=3;}  \
}


/* this had a few problems, fixed output_*logue in m68k.c instead */
#if 0
#define FUNCTION_PROLOGUE(FILE, SIZE)                                 \
{                                                                     \
  register int regno;                                                 \
  register int mask = 0;                                              \
  int num_saved_regs = 0, first = 1;                                  \
  extern char call_used_regs[];                                       \
  int fsize = ((SIZE) + 3) & -4;                                      \
                                                                      \
                                                                      \
  if (frame_pointer_needed)                                           \
    {                                                                 \
      /* Adding negative number is faster on the 68040.  */           \
      if (fsize < 0x8000 && !TARGET_68040)                            \
	{                                                             \
	  fprintf (FILE, "\tlink %s,#%d\n", 	                      \
		       reg_names[FRAME_POINTER_REGNUM], -fsize);      \
	}                                                             \
      else if (TARGET_68020)                                          \
	{                                                             \
	  fprintf (FILE, "\tlink %s,#%d\n",	                      \
		       reg_names[FRAME_POINTER_REGNUM], -fsize);      \
	}                                                             \
      else                                                            \
	{                                                             \
	  fprintf (FILE, "\tlink %s,#0\n\tadd.l #%d,sp\n",	      \
		       reg_names[FRAME_POINTER_REGNUM], -fsize);      \
	}							      \
    }								      \
  else if (fsize)						      \
    {								      \
      /* Adding negative number is faster on the 68040.  */	      \
      if (fsize + 4 < 0x8000)					      \
	{							      \
	  fprintf (FILE, "\tadd.w #%d,sp\n", - (fsize + 4));	      \
	}							      \
      else							      \
	{							      \
	  fprintf (FILE, "\tadd.l #%d,sp\n", - (fsize + 4));          \
	}							      \
    }								      \
  if (TARGET_68881)                                                    \
   {                                                                  \
    for (regno = 23; regno >= 16; regno--)                            \
      if (regs_ever_live[regno] && ! call_used_regs[regno])           \
        if (first) {						      \
          fprintf (FILE, "\tfmovem.x %s", reg_names[regno]);          \
	  first = 0;						      \
         }							      \
        else fprintf (FILE, "/%s", reg_names[regno]);                 \
    if (!first) fprintf (FILE, ",-(sp)\n");			      \
   }								      \
  mask = 0;							      \
  for (regno = 0; regno < 16; regno++)				      \
    if (regs_ever_live[regno] && ! call_used_regs[regno])	      \
      {								      \
        mask |= 1 << (15 - regno);				      \
        num_saved_regs++;                   			      \
      }                                                               \
  if (frame_pointer_needed)                                           \
    {                                                                 \
      mask &= ~ (1 << (15 - FRAME_POINTER_REGNUM));                   \
      num_saved_regs--;                                               \
    }                                                                 \
                                                                      \
                                                                      \
  if (num_saved_regs <= 2)                                            \
    {                                                                 \
      /* Store each separately in the same order moveml uses.         \
         Using two movel instructions instead of a single moveml      \
         is about 15% faster for the 68020 and 68030 at no expense    \
         in code size */                                              \
                                                                      \
      int i;                                                          \
                                                                      \
      /* Undo the work from above. */                                 \
      for (i = 0; i< 16; i++)                                         \
        if (mask & (1 << i))                                          \
          fprintf (FILE, "\tmove.l %s,-(sp)\n", reg_names[15 - i]);   \
    }                                                                 \
  else if (mask)                                                      \
    {                                                                 \
      first = 1;                                                      \
      for (regno = 0; regno < 16; regno++)                            \
        if (mask & (1 << regno))                                      \
          if (first) {                                                \
            fprintf (FILE, "\tmovem.l %s", reg_names[15 - regno]);    \
            first = 0;                                                \
           }                                                          \
          else fprintf (FILE, "/%s", reg_names[15 - regno]);	      \
      fprintf (FILE, ",-(sp)\n");	           		      \
    }                                                                 \
  if (flag_pic && current_function_uses_pic_offset_table)             \
    {                                                                 \
      fprintf (FILE, "\tmove.l #__GLOBAL_OFFSET_TABLE_, %s\n",        \
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);               \
      fprintf (FILE, "\tlea.l (pc,%s.l),%s\n",                        \
		   reg_names[PIC_OFFSET_TABLE_REGNUM],                \
		   reg_names[PIC_OFFSET_TABLE_REGNUM]);               \
    }                                                                 \
}


#define FUNCTION_EPILOGUE(FILE, SIZE)                                 \
{                                                                     \
  register int regno;                                                 \
  register int mask, fmask;                                           \
  register int nregs;                                                 \
  int offset, foffset, fpoffset, first = 1;		              \
  extern char call_used_regs[];                                       \
  int fsize = ((SIZE) + 3) & -4;                                      \
  int big = 0;                                                        \
  rtx insn = get_last_insn ();                                        \
                                                                      \
  /* If the last insn was a BARRIER, we don't have to write any code.  */ \
  if (GET_CODE (insn) == NOTE)                                        \
    insn = prev_nonnote_insn (insn);                                  \
  if (insn && GET_CODE (insn) == BARRIER)                             \
    {                                                                 \
      /* Output just a no-op so that debuggers don't get confused     \
	 about which function the pc is in at this address.  */       \
      fprintf (FILE, "\tnop\n");                                      \
      return;                                                         \
    }                                                                 \
                                                                      \
  nregs = 0;  fmask = 0; fpoffset = 0;                                \
  for (regno = 16; regno < 24; regno++)                               \
    if (regs_ever_live[regno] && ! call_used_regs[regno])             \
      {                                                               \
        nregs++;                                                      \
	fmask |= 1 << (23 - regno);                                   \
      }                                                               \
  foffset = fpoffset + nregs * 12;                                    \
  nregs = 0;  mask = 0;                                               \
  if (frame_pointer_needed)                                           \
    regs_ever_live[FRAME_POINTER_REGNUM] = 0;                         \
  for (regno = 0; regno < 16; regno++)                                \
    if (regs_ever_live[regno] && ! call_used_regs[regno])             \
      {                                                               \
        nregs++;                                                      \
	mask |= 1 << regno;                                           \
      }                                                               \
  offset = foffset + nregs * 4;                                       \
  if (offset + fsize >= 0x8000                                        \
      && frame_pointer_needed                                         \
      && (mask || fmask || fpoffset))                                 \
    {                                                                 \
      fprintf (FILE, "\tmove.l #%d,a0\n", -fsize);                    \
      fsize = 0, big = 1;                                             \
    }                                                                 \
  if (nregs <= 2)                                                     \
    {                                                                 \
      /* Restore each separately in the same order moveml does.       \
         Using two movel instructions instead of a single moveml      \
         is about 15% faster for the 68020 and 68030 at no expense    \
         in code size. */                                             \
                                                                      \
      int i;                                                          \
                                                                      \
      /* Undo the work from above. */                                 \
      for (i = 0; i< 16; i++)                                         \
        if (mask & (1 << i))                                          \
          {                                                           \
            if (big)                                                  \
	      {                                                       \
		fprintf (FILE, "\tmove.l -%d(%s,a0.l),%s\n",          \
			     offset + fsize,                          \
			     reg_names[FRAME_POINTER_REGNUM],         \
			     reg_names[i]);                           \
	      }                                                       \
            else if (! frame_pointer_needed)                          \
	      {                                                       \
		fprintf (FILE, "\tmove.l (sp)+,%s\n",                 \
			     reg_names[i]);                           \
	      }                                                       \
            else                                                      \
	      {                                                       \
		fprintf (FILE, "\tmove.l -%d(%s),%s\n",               \
			     offset + fsize,                          \
			     reg_names[FRAME_POINTER_REGNUM],         \
			     reg_names[i]);                           \
	      }                                                       \
            offset = offset - 4;                                      \
          }                                                           \
    }                                                                 \
  else if (mask)                                                      \
    {                                                                 \
      first = 1;						      \
      for (regno = 0; regno < 16; regno++)                            \
        if (mask & (1 << regno))                                      \
          if (first && big) {                                         \
            fprintf (FILE, "\tmovem.l -%d(%s,a0.l),%s",               \
		     offset + fsize,                                  \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
            first = 0;                                                \
           }                                                          \
          else if (first && ! frame_pointer_needed) {                 \
            fprintf (FILE, "\tmovem.l (sp)+,%s",                      \
		     reg_names[regno]); /* FIX: RZ*/                  \
            first = 0;                                                \
           }                                                          \
          else if (first) {   				              \
            fprintf (FILE, "\tmovem.l -%d(%s),%s",                    \
		     offset + fsize,                                  \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
            first = 0;                                                \
           }                                                          \
          else  						      \
	    fprintf (FILE, "/%s", reg_names[regno]);	              \
      fprintf (FILE, "\n");	                 		      \
    }                                                                 \
  if (fmask && TARGET_68881)                                          \
    {                                                                 \
      first = 1;						      \
      for (regno = 16; regno < 24; regno++)                           \
        if (fmask & (1 << (23 - regno)))                              \
          if (first && big) {	                                      \
            fprintf (FILE, "\tfmovem.x -%d(%s,a0.l),%s",              \
		     foffset + fsize,                                 \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
	    first = 0;						      \
           }                                                          \
          else if (first && ! frame_pointer_needed) {                 \
            fprintf (FILE, "\tfmovem.x (sp)+,%s",                     \
		     reg_names[regno]); /* FIX: RZ */                 \
	    first = 0;						      \
           }                                                          \
          else if (first) {    				              \
            fprintf (FILE, "\tfmovem.x -%d(%s),%s",                   \
		     foffset + fsize,                                 \
		     reg_names[FRAME_POINTER_REGNUM],                 \
		     reg_names[regno]);                               \
	    first = 0;						      \
           }                                                          \
	  else fprintf (FILE, "/%s", reg_names[regno]); 	      \
      fprintf (FILE, "\n");					      \
    }                                                                 \
  if (frame_pointer_needed)                                           \
    fprintf (FILE, "\tunlk %s\n",                                     \
	     reg_names[FRAME_POINTER_REGNUM]);                        \
  else if (fsize)                                                     \
    {                                                                 \
      if (fsize + 4 < 0x8000)                                         \
	{                                                             \
	  fprintf (FILE, "\tadd.w #%d,sp\n", fsize + 4);              \
	}                                                             \
      else                                                            \
	{                                                             \
	  fprintf (FILE, "\tadd.l #%d,sp\n", fsize + 4);              \
	}                                                             \
    }                                                                 \
  if (current_function_pops_args)                                     \
    fprintf (FILE, "\trtd #%d\n", current_function_pops_args);        \
  else                                                                \
    fprintf (FILE, "\trts\n");                                        \
}
#endif  /* unused */

#if 1
#define FUNCTION_PROFILER(FILE, LABEL_NO) \
   fprintf (FILE, "\tmove.l #LP%d,d0\n\tjsr mcount\n", (LABEL_NO));
#endif

#define REGISTER_NAMES \
{"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",	\
 "a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp",	\
 "fp0", "fp1", "fp2", "fp3", "fp4", "fp5", "fp6", "fp7"}

#define ADDITIONAL_REGISTER_NAMES { "fp", 14, "a7", 15 }


/* This is how to output an insn to push a register on the stack.
   It need not be very fast code (profiler) */
#if 0
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmove.l %s,-(sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmove.l (sp)+,%s\n", reg_names[REGNO])
#endif


#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { long l[2];char dstr[30];						\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);			\
     fprintf (FILE, IS_GWASS ?"\tDC.L $%lx,$%lx" :"\t.data4 0x%lx,0x%lx", l[0], l[1]);	\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
     asm_fprintf (FILE, "\t ; %I0r%s\n", dstr);				\
   } while (0)

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, IS_GWASS?"\tDS.B %u\n":"\t.space %u\n", (SIZE))


/* FIX: is it really local? Store in OUTPUT a string (made with alloca)
   containing an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 13),	\
  sprintf ((OUTPUT), "%s%d", (NAME), (LABELNO)))

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { long l;						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
     fprintf (FILE, IS_GWASS?"DC.L $%lx\n":"\t.data4 0x%lx\n", l);		\
   } while (0)


/* FIX: Output a float value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)			\
 do {									\
      if (CODE == 'f')							\
        {								\
          char dstr[30];						\
          REAL_VALUE_TO_DECIMAL (VALUE, "%.9e", dstr);			\
          if (REAL_VALUE_ISINF (VALUE) || REAL_VALUE_ISNAN (VALUE))	\
	    {								\
	      if (REAL_VALUE_NEGATIVE (VALUE))				\
		fprintf (FILE, "#!-99e999");				\
	      else							\
		fprintf (FILE, "#!99e999");				\
	    }								\
          else if (REAL_VALUE_MINUS_ZERO (VALUE))			\
            fprintf (FILE, "#!-0.0");					\
          else								\
            fprintf (FILE, "#!%s", dstr); 				\
        }								\
      else								\
        {								\
          long l;							\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);			\
          fprintf (FILE, "#$%lx", l);					\
        }								\
     } while (0)

/* FIX: not used? Output a double value (represented as a C double) as an
   immediate operand. This macro is a 68k-specific macro.  */
#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { char dstr[30];							\
      REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", dstr );			\
      if (REAL_VALUE_ISINF (VALUE) || REAL_VALUE_ISNAN (VALUE))		\
	{								\
        if (REAL_VALUE_NEGATIVE (VALUE))				\
          fprintf (FILE, "#!-99e9999");					\
        else								\
          fprintf (FILE, "#!99e9999");					\
	}								\
      else if (REAL_VALUE_MINUS_ZERO (VALUE))				\
          fprintf (FILE, "#!-0.0");					\
      else								\
          fprintf (FILE, "#!%s", dstr);				\
    } while (0)

/* FIX: not used? Note, long double immediate operands are not actually
   generated by m68k.md.  */
#undef ASM_OUTPUT_LONG_DOUBLE_OPERAND
#define ASM_OUTPUT_LONG_DOUBLE_OPERAND(FILE,VALUE)			\
 do { char dstr[30];							\
      REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
      asm_fprintf (FILE, "%I0r%s", dstr);				\
    } while (0)

#undef ASM_OUTPUT_LONG_DOUBLE
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  				\
do { long l[3];char dstr[30];						\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);			\
     fprintf (FILE, IS_GWASS ?"\t.DC.L $%lx,$%lx,$%lx":"\t.data4 0x%lx,0x%lx,0x%lx", l[0], l[1], l[2]);	\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
     asm_fprintf (FILE, "\t ; %I0r%s\n", dstr);				\
   } while (0)


#undef PRINT_OPERAND_ADDRESS

/* hacked to suit QDOS conventions better, might need more finetuning
   old version left below, see also crds.h */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx reg1, reg2, breg, ireg;					\
  register rtx addr = ADDR;						\
  rtx offset;								\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "(%s)", reg_names[REGNO (addr)]);			\
      break;								\
    case PRE_DEC:							\
      fprintf (FILE, "-(%s)", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    case POST_INC:							\
      fprintf (FILE, "(%s)+", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    case PLUS:								\
      reg1 = 0;	reg2 = 0;						\
      ireg = 0;	breg = 0;						\
      offset = 0;							\
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))				\
	{								\
	  offset = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))			\
	{								\
	  offset = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) != PLUS) ; /* ?????? */			\
      else if (GET_CODE (XEXP (addr, 0)) == SIGN_EXTEND)		\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == SIGN_EXTEND)		\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT		\
	  || GET_CODE (addr) == SIGN_EXTEND)				\
	{ if (reg1 == 0) reg1 = addr; else reg2 = addr; addr = 0; }	\
      if (offset != 0) { if (addr != 0) abort (); addr = offset; }	\
      if ((reg1 && (GET_CODE (reg1) == SIGN_EXTEND			\
		    || GET_CODE (reg1) == MULT))			\
	  || (reg2 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg2))))		\
	{ breg = reg2; ireg = reg1; }					\
      else if (reg1 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg1)))		\
	{ breg = reg1; ireg = reg2; }					\
      if (ireg != 0 && breg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { int scale = 1;						\
	  if (GET_CODE (ireg) == MULT)					\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (GET_CODE (ireg) == SIGN_EXTEND)				\
	    fprintf (FILE, "L%d(pc,%s.w",				\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "L%d(pc,%s.l",				\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  putc (')', FILE);						\
	  break; }							\
      if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { fprintf (FILE, "L%d(pc,%s.l",				        \
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (breg)]);				\
	  putc (')', FILE);						\
	  break; }							\
      if (ireg != 0 || breg != 0)					\
	{ int scale = 1;						\
	  if (breg == 0)						\
	    abort ();							\
	  if (addr && GET_CODE (addr) == LABEL_REF) abort ();		\
	  if (addr != 0) 						\
	    output_addr_const (FILE, addr);				\
          else if (ireg != 0) putc('0',FILE);                           \
	  putc ('(', FILE); 						\
	  fprintf (FILE, "%s", reg_names[REGNO (breg)]);		\
	  if (ireg != 0)						\
	    putc (',', FILE);						\
	  if (ireg != 0 && GET_CODE (ireg) == MULT)			\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (ireg != 0 && GET_CODE (ireg) == SIGN_EXTEND)		\
	    fprintf (FILE, "%s.w", reg_names[REGNO (XEXP (ireg, 0))]);	\
	  else if (ireg != 0)						\
	    fprintf (FILE, "%s.l", reg_names[REGNO (ireg)]);		\
	  if (scale != 1) fprintf (FILE, "*%d", scale);			\
	  putc (')', FILE);						\
	  break;							\
	}								\
      else if (reg1 != 0 && GET_CODE (addr) == LABEL_REF)		\
	{ fprintf (FILE, "L%d(pc,%s.l)",				\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (reg1)]);				\
	  break; }							\
    default:								\
      if (GET_CODE (addr) == CONST_INT					\
	  && INTVAL (addr) < 0x8000					\
	  && INTVAL (addr) >= -0x8000)					\
	fprintf (FILE, "%d", INTVAL (addr));				\
      else								\
        output_addr_const (FILE, addr);					\
    }}


#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    unsigned char *_hide_p = (unsigned char *) (MYSTRING);		      \
    int _hide_thissize = (MYLENGTH);					      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      unsigned char *p = _hide_p;					      \
      int thissize = _hide_thissize;					      \
      int i,first=1;							      \
                                                                              \
      fprintf(asm_out_file,(IS_GWASS)?"\tDC.B ":"\t.data1\t");                \
      for (i = 0; i < thissize; i++)					      \
	{								      \
	  register int c = p[i];					      \
	  fprintf(asm_out_file,IS_GWASS ?"%s$%X":"%s0x%x",(first?"":","),c);  \
          first=0;                                                            \
        }                                                                     \
      fprintf(asm_out_file,"\n");   \
  }}									      \
  while (0)


#else /* Using GAS, which uses the MIT assembler syntax, like a Sun.  */

#define FUNCTION_PROFILER(FILE, LABEL_NO) \
   fprintf (FILE, "\tmovl #LP%d,d0\n\tjsr mcount\n", (LABEL_NO));

#endif /* MOTOROLA */
