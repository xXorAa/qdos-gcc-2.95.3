#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "expr.h"
#include "output.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "defaults.h"
#include "real.h"
#include "toplev.h"
#include "dbxout.h"
#include "sdbout.h"


/* code for exact match, currently not needed*/
#if 0
static struct {
  int len;
  char *name;
  tree id;
} c68_specials []= {
#ifdef C68_INTMATH
  {0,MULSI3_LIBCALL,NULL},
  {0,UMULSI3_LIBCALL,NULL},
  {0,DIVSI3_LIBCALL,NULL},
  {0,UDIVSI3_LIBCALL,NULL},
  {0,MODSI3_LIBCALL,NULL},
  {0,UMODSI3_LIBCALL,NULL},
#endif
  {0,NULL,NULL}
};

static int c68_scount=0;

static void init_c68_specials()
{
  int i=0;
  for(;c68_specials[i].name;i++)
    {
      c68_specials[i].len=strlen(c68_specials[i].name);
    }
  c68_scount=i;
}
#endif

static int is_c68_lib(id)
     tree id;
{
  int i;
  
#if 0
  if (c68_scount==0)
    init_c68_specials();
#endif

#if 0  
  printf("is_c68_lib: %s\n",IDENTIFIER_POINTER(id));
  printf("c68_scount %d\n",c68_scount);
#endif

  if ( IDENTIFIER_LENGTH(id)>3 &&
       ( !bcmp(IDENTIFIER_POINTER(id),"*.X",3) ||
	 !bcmp(IDENTIFIER_POINTER(id),"*.Y",3)))
    return 1;
  else return 0;


  /* old code, used exact match */
#if 0
  for(i=0; i<c68_scount; i++)
    {
      if ( IDENTIFIER_LENGTH(id)==c68_specials[i].len )
	{ 
	  /* try hashed value compare first */
	  if ( id == c68_specials[i].id )
	    return 1;
	  /* if id not initialised try full compare */
	  if (!c68_specials[i].id && !bcmp(IDENTIFIER_POINTER(id),
		    c68_specials[i].name,
		    c68_specials[i].len))
	    { 
	      /* add hash id for speedup */
	      /*printf("addind hashed id for %s\n",IDENTIFIER_POINTER(id));*/
	      c68_specials[i].id=id;
	      return 1;
	    }   
	}
    }

  return 0;
#endif /* old code */
}

int qdos_return_pops_args(fundecl,funtype,size)
     tree fundecl;
     tree funtype;
     int size;
{
  if (fundecl && TREE_CODE (fundecl) == IDENTIFIER_NODE
      && is_c68_lib(fundecl))
    {
      /*printf("..is_c68_lib\n");*/
      return size;
    }
  else
    /* fallback to default m68k conventions */
    return  ((TARGET_RTD && (!(fundecl) || TREE_CODE (fundecl) != IDENTIFIER_NODE)
	      && (TYPE_ARG_TYPES (funtype) == 0
		  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (funtype)))
		      == void_type_node)))
	     ? (size) : 0);
}



qdos_asm_output_labelref(file, name)
     FILE *file;
     char *name;
{
  asm_fprintf (file, "%0U%s", name);
}

qdos_asm_output_common(file,name,size,rounded)
     FILE* file;
     char *name;
     int size,rounded;
{
  bss_section();
  fputs ((IS_GWASS ? "    XDEF " :"\t.extern\t"), (file));
  assemble_name ((file), (name)), fputs("\n",(file));
  assemble_name ((file), (name)), fputs(":\n",(file));
  if (IS_GWASS)
    fprintf((file),"    DS.%s %d\n",((size) % 2) ? "b" : "w",
	                              ((size) % 2) ? (size) : (size)/2 ); 
  else {
    fprintf ((file), "\t.space\t%u\n", (size));
    fputs((size)!=(rounded) ?"\t.align\t2\n":"",(file));
  }
}
qdos_asm_output_local(file,name,size,rounded)
     FILE* file;
     char *name;
     int size,rounded;
{
  bss_section();
  assemble_name ((file), (name)), fputs(":\n",(file));
  if (IS_GWASS)
    fprintf((file),"    DS.%s %d\n",((size) % 2) ? "B" : "W",
	                              ((size) % 2) ? (size) : (size)/2 ); 
  else {
    fprintf ((file), "\t.space\t%u\n", (size));
    fputs((size)!=(rounded) ?"\t.align\t2\n":"",(file));
  }
}

qdos_asm_output_align(file,log)
     FILE *file;
     int log;
{
  if (IS_GWASS)
    {
      if (log==2)
	fprintf (file, "    ALIGN4\n");
      else if (log==3)
	fprintf (file, "    ALIGN8\n");
      else if (log>3)
	fprintf (file, "    ALIGN16\n");
    }
  else
    { 
      int _LOG = log;
      if (_LOG == 1)     
	fprintf (file, "\t.align 2\n"); 
      else if (_LOG == 2)
	fprintf (file, "\t.align 4\n");
      else if (_LOG != 0)
	fprintf (file, "\t.align 8\n");\
    }
}

qdos_asm_start_file(file)
     FILE *file;
{
  if( IS_GWASS)
    fprintf (file, "\tSECTION\ttext\n\tSECTION\trom\n\tSECTION\tdata\n\tSECTION\tbss\n\n");
  else
    fprintf (file, "\t.sect\t.text\n\t.sect\t.rom\n\t.sect\t.data\n\t.sect\t.bss\n\n");
}

