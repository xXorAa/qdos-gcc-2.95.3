/* libgcc1 routines for 68000 w/o floating-point hardware.
   Copyright (C) 1994, 1996, 1997, 1998 Free Software Foundation, Inc.

   motorola syntax version 
	
This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* Use this one for any 680x0; assumes no floating point hardware.
   The trailing " '" appearing on some lines is for ANSI preprocessors.  Yuk.
   Some of this code comes from MINIX, via the folks at ericsson.
   D. V. Henkel-Wallace (gumby@cygnus.com) Fete Bastille, 1992
*/

/* These are predefined by new versions of GNU cpp.  */

#ifndef __USER_LABEL_PREFIX__
#define __USER_LABEL_PREFIX__ _
#endif

#ifndef __REGISTER_PREFIX__
#define __REGISTER_PREFIX__
#endif

#ifndef __IMMEDIATE_PREFIX__
#define __IMMEDIATE_PREFIX__ #
#endif

/* ANSI concatenation macros.  */

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

/* Use the right prefix for global labels.  */

#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

/* Use the right prefix for immediate values.  */

#define IMM(x) CONCAT1 (__IMMEDIATE_PREFIX__, x)



#ifdef L_floatex

#if 0	
;| This is an attempt at a decent floating point (single, double and 
;| extended double) code for the GNU C compiler. It should be easy to
;| adapt to other compilers (but beware of the local labels!).

;| Starting date: 21 October, 1990

;| It is convenient to introduce the notation (s,e,f) for a floating point
;| number, where s=sign, e=exponent, f=fraction. We will call a floating
;| point number fpn to abbreviate, independently of the precision.
;| Let MAX_EXP be in each case the maximum exponent (255 for floats, 1023 
;| for doubles and 16383 for long doubles). We then have the following 
;| different cases:
;|  1. Normalized fpns have 0 < e < MAX_EXP. They correspond to 
;|     (-1)^s x 1.f x 2^(e-bias-1).
;|  2. Denormalized fpns have e=0. They correspond to numbers of the form
;|     (-1)^s x 0.f x 2^(-bias).
;|  3. +/-INFINITY have e=MAX_EXP, f=0.
;|  4. Quiet NaN (Not a Number) have all bits set.
;|  5. Signaling NaN (Not a Number) have s=0, e=MAX_EXP, f=1.

;|=============================================================================
;|                                  exceptions
;|=============================================================================

;| This is the floating point condition code register (_fpCCR):
;|
;| struct {
;|   short _exception_bits;	
;|   short _trap_enable_bits;	
;|   short _sticky_bits;
;|   short _rounding_mode;
;|   short _format;
;|   short _last_operation;
;|   union {
;|     float sf;
;|     double df;
;|   } _operand1;
;|   union {
;|     float sf;
;|     double df;
;|   } _operand2;
;| } _fpCCR;
#endif
	
	.data
	.even

	.globl	SYM (_fpCCR)
	
SYM (_fpCCR):
__exception_bits:
	.data2	0
__trap_enable_bits:
	.data2	0
__sticky_bits:
	.data2	0
__rounding_mode:
	.data2	ROUND_TO_NEAREST
__format:
	.data2	NIL
__last_operation:
	.data2	NOOP
__operand1:
	.data4	0
	.data4	0
__operand2:
	.data4 	0
	.data4	0

/*;| Offsets:*/
EBITS  = __exception_bits - SYM (_fpCCR)
TRAPE  = __trap_enable_bits - SYM (_fpCCR)
STICK  = __sticky_bits - SYM (_fpCCR)
ROUND  = __rounding_mode - SYM (_fpCCR)
FORMT  = __format - SYM (_fpCCR)
LASTO  = __last_operation - SYM (_fpCCR)
OPER1  = __operand1 - SYM (_fpCCR)
OPER2  = __operand2 - SYM (_fpCCR)

/*;| The following exception types are supported:*/
INEXACT_RESULT 		= 0x0001
UNDERFLOW 		= 0x0002
OVERFLOW 		= 0x0004
DIVIDE_BY_ZERO 		= 0x0008
INVALID_OPERATION 	= 0x0010

/*;| The allowed rounding modes are:*/
UNKNOWN           = -1
ROUND_TO_NEAREST  = 0 ;| round result to nearest representable value
ROUND_TO_ZERO     = 1 ;| round result towards zero
ROUND_TO_PLUS     = 2 ;| round result towards plus infinity
ROUND_TO_MINUS    = 3 ;| round result towards minus infinity

;| The allowed values of format are:
NIL          = 0
SINGLE_FLOAT = 1
DOUBLE_FLOAT = 2
LONG_FLOAT   = 3

;| The allowed values for the last operation are:
NOOP         = 0
ADD          = 1
MULTIPLY     = 2
DIVIDE       = 3
NEGATE       = 4
COMPARE      = 5
EXTENDSFDF   = 6
TRUNCDFSF    = 7

	
;|=============================================================================
;|                           __clear_sticky_bits
;|=============================================================================

;| The sticky bits are normally not cleared (thus the name), whereas the 
;| exception type and exception value reflect the last computation. 
;| This routine is provided to clear them (you can also write to _fpCCR,
;| since it is globally visible).

	.globl  SYM (__clear_sticky_bit)

	.text
	.even

;| void __clear_sticky_bits(void);
SYM (__clear_sticky_bit):		
	lea	SYM (_fpCCR),a0
#ifndef __mcf5200__
	move.w	IMM (0),STICK(a0)
#else
	clr.w	STICK(a0)
#endif
	rts

;|=============================================================================
;|                           _exception_handler
;|=============================================================================

	.globl  _exception_handler

	.text
	.even

;| This is the common exit point if an exception occurs.
;| NOTE: it is NOT callable from C!
;| It expects the exception type in d7, the format (SINGLE_FLOAT,
;| DOUBLE_FLOAT or LONG_FLOAT) in d6, and the last operation code in d5.
;| It sets the corresponding exception and sticky bits, and the format. 
;| Depending on the format if fills the corresponding slots for the 
;| operands which produced the exception (all this information is provided
;| so if you write your own exception handlers you have enough information
;| to deal with the problem).
;| Then checks to see if the corresponding exception is trap-enabled, 
;| in which case it pushes the address of _fpCCR and traps through 
;| trap FPTRAP (15 for the moment).

FPTRAP = 15

_exception_handler:
	lea	SYM (_fpCCR),a0
	move.w	d7,EBITS(a0)	;| set __exception_bits
#ifndef __mcf5200__
	or.w	d7,STICK(a0)	;| and __sticky_bits
#else
	move.w	STICK(a0),d4
	or.l	d7,d4
	move.w	d4,STICK(a0)
#endif
	move.w	d6,FORMT(a0)	;| and __format
	move.w	d5,LASTO(a0)	;| and __last_operation

;| Now put the operands in place:
#ifndef __mcf5200__
	cmp.w	IMM (SINGLE_FLOAT),d6
#else
	cmp.l	IMM (SINGLE_FLOAT),d6
#endif
	beq	1f
	move.l	8(a6),OPER1(a0)
	move.l	12(a6),OPER1+4(a0)
	move.l	16(a6),OPER2(a0)
	move.l	20(a6),OPER2+4(a0)
	bra	2f
1:	move.l	8(a6),OPER1(a0)
	move.l	12(a6),OPER2(a0)
2:
;| And check whether the exception is trap-enabled:
#ifndef __mcf5200__
	and.w	TRAPE(a0),d7	;| is exception trap-enabled?
#else
	clr.l	d6
	move.w	TRAPE(a0),d6
	and.l	d6,d7
#endif
	beq	1f		;| no, exit
	pea	SYM (_fpCCR)	;| yes, push address of _fpCCR
	trap	IMM (FPTRAP)	;| and trap
#ifndef __mcf5200__
1:	movem.l	(sp)+,d2-d7	;| restore data registers
#else
1:	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| and return
	rts
#endif /* L_floatex */

#ifdef  L_mulsi3
	.text
/*	.proc*/
	.globl	SYM (__mulsi3)
SYM (__mulsi3):
	move.w	4(sp),d0	/* x0 -> d0 */
	mulu.w	10(sp),d0	/* x0*y1 */
	move.w	6(sp),d1	/* x1 -> d1 */
	mulu.w	8(sp),d1	/* x1*y0 */
#ifndef __mcf5200__
	add.w	d1,d0
#else
	add.l	d1,d0
#endif
	swap	d0
	clr.w	d0
	move.w	6(sp),d1	/* x1 -> d1 */
	mulu.w	10(sp),d1	/* x1*y1 */
	add.l	d1,d0

	rts
#endif /* L_mulsi3 */

#ifdef  L_udivsi3
	.text
	/*.proc*/
	.globl	SYM (__udivsi3)
SYM (__udivsi3):
#ifndef __mcf5200__
	move.l	d2,-(sp)
	move.l	12(sp),d1	/* d1 = divisor */
	move.l	8(sp),d0	/* d0 = dividend */

	cmp.l	IMM (0x10000),d1 /* divisor >= 2 ^ 16 ?   */
	bcc	L3		/* then try next algorithm */
	move.l	d0,d2
	clr.w	d2
	swap	d2
	divu	d1,d2          /* high quotient in lower word */
	move.w	d2,d0		/* save high quotient */
	swap	d0
	move.w	10(sp),d2	/* get low dividend + high rest */
	divu	d1,d2		/* low quotient */
	move.w	d2,d0
	bra	L6

L3:	move.l	d1,d2		/* use d2 as divisor backup */
L4:	lsr.l	IMM (1),d1	/* shift divisor */
	lsr.l	IMM (1),d0	/* shift dividend */
	cmp.l	IMM (0x10000),d1 /* still divisor >= 2 ^ 16 ?  */
	bcc	L4
	divu	d1,d0		/* now we have 16 bit divisor */
	and.l	IMM (0xffff),d0 /* mask out divisor, ignore remainder */

/* Multiply the 16 bit tentative quotient with the 32 bit divisor.  Because of
   the operand ranges, this might give a 33 bit product.  If this product is
   greater than the dividend, the tentative quotient was too large. */
	move.l	d2,d1
	mulu	d0,d1		/* low part, 32 bits */
	swap	d2
	mulu	d0,d2		/* high part, at most 17 bits */
	swap	d2		/* align high part with low part */
	tst.w	d2		/* high part 17 bits? */
	bne	L5		/* if 17 bits, quotient was too large */
	add.l	d2,d1		/* add parts */
	bcs	L5		/* if sum is 33 bits, quotient was too large */
	cmp.l	8(sp),d1	/* compare the sum with the dividend */
	bls	L6		/* if sum > dividend, quotient was too large */
L5:	subq.l	IMM (1),d0	/* adjust quotient */

L6:	move.l	(sp)+,d2
	rts

#else /* __mcf5200__ */

/* Coldfire implementation of non-restoring division algorithm from
   Hennessy & Patterson, Appendix A. */
	link	a6,IMM (-12)
	movem.l	d2-d4,(sp)
	move.l	8(a6),d0
	move.l	12(a6),d1
	clr.l	d2		;| clear p
	moveq	IMM (31),d4
L1:	add.l	d0,d0		;| shift reg pair (p,a) one bit left
	addx.l	d2,d2
	move.l	d2,d3		;| subtract b from p, store in tmp.
	sub.l	d1,d3
	bcs	L2		;| if no carry,
	bset	IMM (0),d0	;| set the low order bit of a to 1,
	move.l	d3,d2		;| and store tmp in p.
L2:	subq.l	IMM (1),d4
	bcc	L1
	movem.l	(sp),d2-d4	;| restore data registers
	unlk	a6		;| and return
	rts
#endif /* __mcf5200__ */

#endif /* L_udivsi3 */

#ifdef  L_divsi3
	.text
	/*.proc*/
	.globl	SYM (__divsi3)
SYM (__divsi3):
	move.l	d2,-(sp)

	moveq	IMM (1),d2	/* sign of result stored in d2 (=1 or =-1) */
	move.l	12(sp),d1	/* d1 = divisor */
	bpl	L1
	neg.l	d1
#ifndef __mcf5200__
	neg.b	d2		/* change sign because divisor <0  */
#else
	neg.l	d2		/* change sign because divisor <0  */
#endif
L1:	move.l	8(sp),d0	/* d0 = dividend */
	bpl	L2
	neg.l	d0
#ifndef __mcf5200__
	neg.b	d2
#else
	neg.l	d2
#endif

L2:	move.l	d1,-(sp)
	move.l	d0,-(sp)
	bsr	SYM (__udivsi3)	/* divide abs(dividend) by abs(divisor) */
	addq.l	IMM (8),sp

	tst.b	d2
	bpl	L3
	neg.l	d0

L3:	move.l	(sp)+,d2
	rts
#endif /* L_divsi3 */

#ifdef  L_umodsi3
	.text
	/*.proc*/
	.globl	SYM (__umodsi3)
SYM (__umodsi3):
	move.l	8(sp),d1	/* d1 = divisor */
	move.l	4(sp),d0	/* d0 = dividend */
	move.l	d1,-(sp)
	move.l	d0,-(sp)
	bsr	SYM (__udivsi3)
	addq.l	IMM (8),sp
	move.l	8(sp),d1	/* d1 = divisor */
#ifndef __mcf5200__
	move.l	d1,-(sp)
	move.l	d0,-(sp)
	bsr	SYM (__mulsi3)	/* d0 = (a/b)*b */
	addq.l	IMM (8),sp
#else
	muls.l	d1,d0
#endif
	move.l	4(sp),d1	/* d1 = dividend */
	sub.l	d0,d1		/* d1 = a - (a/b)*b */
	move.l	d1,d0
	rts
#endif /* L_umodsi3 */

#ifdef  L_modsi3
	.text
	/*.proc*/
	.globl	SYM (__modsi3)
SYM (__modsi3):
	move.l	8(sp),d1	/* d1 = divisor */
	move.l	4(sp),d0	/* d0 = dividend */
	move.l	d1,-(sp)
	move.l	d0,-(sp)
	bsr	SYM (__divsi3)
	addq.l	IMM (8),sp
	move.l	8(sp),d1	/* d1 = divisor */
#ifndef __mcf5200__
	move.l	d1,-(sp)
	move.l	d0,-(sp)
	bsr	SYM (__mulsi3)	/* d0 = (a/b)*b */
	addq.l	IMM (8),sp
#else
	muls.l	d1,d0
#endif
	move.l	4(sp),d1	/* d1 = dividend */
	sub.l	d0,d1		/* d1 = a - (a/b)*b */
	move.l	d1,d0
	rts
#endif /* L_modsi3 */


#ifdef  L_double

	.globl	SYM (_fpCCR)
	.globl  _exception_handler

QUIET_NaN      = 0xffffffff

D_MAX_EXP      = 0x07ff
D_BIAS         = 1022
DBL_MAX_EXP    = D_MAX_EXP - D_BIAS
DBL_MIN_EXP    = 1 - D_BIAS
DBL_MANT_DIG   = 53

INEXACT_RESULT 		= 0x0001
UNDERFLOW 		= 0x0002
OVERFLOW 		= 0x0004
DIVIDE_BY_ZERO 		= 0x0008
INVALID_OPERATION 	= 0x0010

DOUBLE_FLOAT = 2

NOOP         = 0
ADD          = 1
MULTIPLY     = 2
DIVIDE       = 3
NEGATE       = 4
COMPARE      = 5
EXTENDSFDF   = 6
TRUNCDFSF    = 7

UNKNOWN           = -1
ROUND_TO_NEAREST  = 0 ;| round result to nearest representable value
ROUND_TO_ZERO     = 1 ;| round result towards zero
ROUND_TO_PLUS     = 2 ;| round result towards plus infinity
ROUND_TO_MINUS    = 3 ;| round result towards minus infinity

;| Entry points:

	.globl SYM (__adddf3)
	.globl SYM (__subdf3)
	.globl SYM (__muldf3)
	.globl SYM (__divdf3)
	.globl SYM (__negdf2)
	.globl SYM (__cmpdf2)

	.text
	.even

;| These are common routines to return and signal exceptions.	

Ldden:
;| Return and signal a denormalized number
	or.l	d7,d0
	move.w	IMM (INEXACT_RESULT+UNDERFLOW),d7
	moveq	IMM (DOUBLE_FLOAT),d6
	jmp	_exception_handler

Ldinfty:
Ldoverflow:
;| Return a properly signed INFINITY and set the exception flags 
	move.l	IMM (0x7ff00000),d0
	move.l	IMM (0),d1
	or.l	d7,d0
	move.w	IMM (INEXACT_RESULT+OVERFLOW),d7
	moveq	IMM (DOUBLE_FLOAT),d6
	jmp	_exception_handler

Ldunderflow:
;| Return 0 and set the exception flags 
	move.l	IMM (0),d0
	move.l	d0,d1
	move.w	IMM (INEXACT_RESULT+UNDERFLOW),d7
	moveq	IMM (DOUBLE_FLOAT),d6
	jmp	_exception_handler

Ldinop:
;| Return a quiet NaN and set the exception flags
	move.l	IMM (QUIET_NaN),d0
	move.l	d0,d1
	move.w	IMM (INEXACT_RESULT+INVALID_OPERATION),d7
	moveq	IMM (DOUBLE_FLOAT),d6
	jmp	_exception_handler

Lddiv0:
;| Return a properly signed INFINITY and set the exception flags
	move.l	IMM (0x7ff00000),d0
	move.l	IMM (0),d1
	or.l	d7,d0
	move.w	IMM (INEXACT_RESULT+DIVIDE_BY_ZERO),d7
	moveq	IMM (DOUBLE_FLOAT),d6
	jmp	_exception_handler

;|=============================================================================
;|=============================================================================
;|                         double precision routines
;|=============================================================================
;|=============================================================================

;| A double precision floating point number (double) has the format:
;|
;| struct _double {
;|  unsigned int sign      : 1;  /* sign bit */ 
;|  unsigned int exponent  : 11; /* exponent, shifted by 126 */
;|  unsigned int fraction  : 52; /* fraction */
;| } double;
;| 
;| Thus sizeof(double) = 8 (64 bits). 
;|
;| All the routines are callable from C programs, and return the result 
;| in the register pair d0-d1. They also preserve all registers except 
;| d0-d1 and a0-a1.

;|=============================================================================
;|                              __subdf3
;|=============================================================================

;| double __subdf3(double, double);
SYM (__subdf3):
	bchg	IMM (31),12(sp) ;| change sign of second operand
				;| and fall through, so we always add
;|=============================================================================
;|                              __adddf3
;|=============================================================================

;| double __adddf3(double, double);
SYM (__adddf3):
#ifndef __mcf5200__
	link	a6,IMM (0)	;| everything will be done in registers
	movem.l	d2-d7,-(sp)	;| save all data registers and a2 (but d0-d1)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.l	8(a6),d0	;| get first operand
	move.l	12(a6),d1	;| 
	move.l	16(a6),d2	;| get second operand
	move.l	20(a6),d3	;| 

	move.l	d0,d7		;| get d0's sign bit in d7 '
	add.l	d1,d1		;| check and clear sign bit of a, and gain one
	addx.l	d0,d0		;| bit of extra precision
	beq	Ladddfb	;| if zero return second operand

	move.l	d2,d6		;| save sign in d6 
	add.l	d3,d3		;| get rid of sign bit and gain one bit of
	addx.l	d2,d2		;| extra precision
	beq	Ladddfa	;| if zero return first operand

	and.l	IMM (0x80000000),d7 ;| isolate a's sign bit '
        swap	d6		;| and also b's sign bit '
#ifndef __mcf5200__
	and.w	IMM (0x8000),d6	;|
	or.w	d6,d7		;| and combine them into d7, so that a's sign '
				;| bit is in the high word and b's is in the '
				;| low word, so d6 is free to be used
#else
	and.l	IMM (0x8000),d6
	or.l	d6,d7
#endif
	move.l	d7,a0		;| now save d7 into a0, so d7 is free to
                		;| be used also

;| Get the exponents and check for denormalized and/or infinity.

	move.l	IMM (0x001fffff),d6 ;| mask for the fraction
	move.l	IMM (0x00200000),d7 ;| mask to put hidden bit back

	move.l	d0,d4		;| 
	and.l	d6,d0		;| get fraction in d0
	not.l	d6		;| make d6 into mask for the exponent
	and.l	d6,d4		;| get exponent in d4
	beq	Ladddfaden	;| branch if a is denormalized
	cmp.l	d6,d4		;| check for INFINITY or NaN
	beq	Ladddfnf       ;| 
	or.l	d7,d0		;| and put hidden bit back
Ladddf1:
	swap	d4		;| shift right exponent so that it starts
#ifndef __mcf5200__
	lsr.w	IMM (5),d4	;| in bit 0 and not bit 20
#else
	lsr.l	IMM (5),d4	;| in bit 0 and not bit 20
#endif
;| Now we have a's exponent in d4 and fraction in d0-d1 '
	move.l	d2,d5		;| save b to get exponent
	and.l	d6,d5		;| get exponent in d5
	beq	Ladddfbden	;| branch if b is denormalized
	cmp.l	d6,d5		;| check for INFINITY or NaN
	beq	Ladddfnf
	not.l	d6		;| make d6 into mask for the fraction again
	and.l	d6,d2		;| and get fraction in d2
	or.l	d7,d2		;| and put hidden bit back
Ladddf2:
	swap	d5		;| shift right exponent so that it starts
#ifndef __mcf5200__
	lsr.w	IMM (5),d5	;| in bit 0 and not bit 20
#else
	lsr.l	IMM (5),d5	;| in bit 0 and not bit 20
#endif

;| Now we have b's exponent in d5 and fraction in d2-d3. '

;| The situation now is as follows: the signs are combined in a0, the 
;| numbers are in d0-d1 (a) and d2-d3 (b), and the exponents in d4 (a)
;| and d5 (b). To do the rounding correctly we need to keep all the
;| bits until the end, so we need to use d0-d1-d2-d3 for the first number
;| and d4-d5-d6-d7 for the second. To do this we store (temporarily) the
;| exponents in a2-a3.

#ifndef __mcf5200__
	movem.l	a2-a3,-(sp)	;| save the address registers
#else
	move.l	a2,-(sp)	
	move.l	a3,-(sp)	
	move.l	a4,-(sp)	
#endif

	move.l	d4,a2		;| save the exponents
	move.l	d5,a3		;| 

	move.l	IMM (0),d7	;| and move the numbers around
	move.l	d7,d6		;|
	move.l	d3,d5		;|
	move.l	d2,d4		;|
	move.l	d7,d3		;|
	move.l	d7,d2		;|

;| Here we shift the numbers until the exponents are the same, and put 
;| the largest exponent in a2.
#ifndef __mcf5200__
	exg	d4,a2		;| get exponents back
	exg	d5,a3		;|
	cmp.w	d4,d5		;| compare the exponents
#else
	move.l	d4,a4		;| get exponents back
	move.l	a2,d4
	move.l	a4,a2
	move.l	d5,a4
	move.l	a3,d5
	move.l	a4,a3
	cmp.l	d4,d5		;| compare the exponents
#endif
	beq	Ladddf3	;| if equal don't shift '
	bhi	9f		;| branch if second exponent is higher

;| Here we have a's exponent larger than b's, so we have to shift b. We do 
;| this by using as counter d2:
1:	move.w	d4,d2		;| move largest exponent to d2
#ifndef __mcf5200__
	sub.w	d5,d2		;| and subtract second exponent
	exg	d4,a2		;| get back the longs we saved
	exg	d5,a3		;|
#else
	sub.l	d5,d2		;| and subtract second exponent
	move.l	d4,a4		;| get back the longs we saved
	move.l	a2,d4
	move.l	a4,a2
	move.l	d5,a4
	move.l	a3,d5
	move.l	a4,a3
#endif
;| if difference is too large we don't shift (actually, we can just exit) '
#ifndef __mcf5200__
	cmp.w	IMM (DBL_MANT_DIG+2),d2
#else
	cmp.l	IMM (DBL_MANT_DIG+2),d2
#endif
	bge	Ladddfbsmall
#ifndef __mcf5200__
	cmp.w	IMM (32),d2	;| if difference >= 32, shift by longs
#else
	cmp.l	IMM (32),d2	;| if difference >= 32, shift by longs
#endif
	bge	5f
2:
#ifndef __mcf5200__
	cmp.w	IMM (16),d2	;| if difference >= 16, shift by words	
#else
	cmp.l	IMM (16),d2	;| if difference >= 16, shift by words	
#endif
	bge	6f
	bra	3f		;| enter dbra loop

4:
#ifndef __mcf5200__
	lsr.l	IMM (1),d4
	roxr.l	IMM (1),d5
	roxr.l	IMM (1),d6
	roxr.l	IMM (1),d7
#else
	lsr.l	IMM (1),d7
	btst	IMM (0),d6
	beq	10f
	bset	IMM (31),d7
10:	lsr.l	IMM (1),d6
	btst	IMM (0),d5
	beq	11f
	bset	IMM (31),d6
11:	lsr.l	IMM (1),d5
	btst	IMM (0),d4
	beq	12f
	bset	IMM (31),d5
12:	lsr.l	IMM (1),d4
#endif
3:
#ifndef __mcf5200__
	dbra	d2,4b
#else
	subq.l	IMM (1),d2
	bpl	4b	
#endif
	move.l	IMM (0),d2
	move.l	d2,d3	
	bra	Ladddf4
5:
	move.l	d6,d7
	move.l	d5,d6
	move.l	d4,d5
	move.l	IMM (0),d4
#ifndef __mcf5200__
	sub.w	IMM (32),d2
#else
	sub.l	IMM (32),d2
#endif
	bra	2b
6:
	move.w	d6,d7
	swap	d7
	move.w	d5,d6
	swap	d6
	move.w	d4,d5
	swap	d5
	move.w	IMM (0),d4
	swap	d4
#ifndef __mcf5200__
	sub.w	IMM (16),d2
#else
	sub.l	IMM (16),d2
#endif
	bra	3b
	
9:
#ifndef __mcf5200__
	exg	d4,d5
	move.w	d4,d6
	sub.w	d5,d6		;| keep d5 (largest exponent) in d4
	exg	d4,a2
	exg	d5,a3
#else
	move.l	d5,d6
	move.l	d4,d5
	move.l	d6,d4
	sub.l	d5,d6
	move.l	d4,a4
	move.l	a2,d4
	move.l	a4,a2
	move.l	d5,a4
	move.l	a3,d5
	move.l	a4,a3
#endif
;| if difference is too large we don't shift (actually, we can just exit) '
#ifndef __mcf5200__
	cmp.w	IMM (DBL_MANT_DIG+2),d6
#else
	cmp.l	IMM (DBL_MANT_DIG+2),d6
#endif
	bge	Ladddfasmall
#ifndef __mcf5200__
	cmp.w	IMM (32),d6	;| if difference >= 32, shift by longs
#else
	cmp.l	IMM (32),d6	;| if difference >= 32, shift by longs
#endif
	bge	5f
2:
#ifndef __mcf5200__
	cmp.w	IMM (16),d6	;| if difference >= 16, shift by words	
#else
	cmp.l	IMM (16),d6	;| if difference >= 16, shift by words	
#endif
	bge	6f
	bra	3f		;| enter dbra loop

4:
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
#else
	lsr.l	IMM (1),d3
	btst	IMM (0),d2
	beq	10f
	bset	IMM (31),d3
10:	lsr.l	IMM (1),d2
	btst	IMM (0),d1
	beq	11f
	bset	IMM (31),d2
11:	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	12f
	bset	IMM (31),d1
12:	lsr.l	IMM (1),d0
#endif
3:
#ifndef __mcf5200__
	dbra	d6,4b
#else
	subq.l	IMM (1),d6
	bpl	4b
#endif
	move.l	IMM (0),d7
	move.l	d7,d6
	bra	Ladddf4
5:
	move.l	d2,d3
	move.l	d1,d2
	move.l	d0,d1
	move.l	IMM (0),d0
#ifndef __mcf5200__
	sub.w	IMM (32),d6
#else
	sub.l	IMM (32),d6
#endif
	bra	2b
6:
	move.w	d2,d3
	swap	d3
	move.w	d1,d2
	swap	d2
	move.w	d0,d1
	swap	d1
	move.w	IMM (0),d0
	swap	d0
#ifndef __mcf5200__
	sub.w	IMM (16),d6
#else
	sub.l	IMM (16),d6
#endif
	bra	3b
Ladddf3:
#ifndef __mcf5200__
	exg	d4,a2	
	exg	d5,a3
#else
	move.l	d4,a4
	move.l	a2,d4
	move.l	a4,a2
	move.l	d5,a4
	move.l	a3,d5
	move.l	a4,a3
#endif
Ladddf4:	
;| Now we have the numbers in d0--d3 and d4--d7, the exponent in a2, and
;| the signs in a4.

;| Here we have to decide whether to add or subtract the numbers:
#ifndef __mcf5200__
	exg	d7,a0		;| get the signs 
	exg	d6,a3		;| a3 is free to be used
#else
	move.l	d7,a4
	move.l	a0,d7
	move.l	a4,a0
	move.l	d6,a4
	move.l	a3,d6
	move.l	a4,a3
#endif
	move.l	d7,d6		;|
	move.w	IMM (0),d7	;| get a's sign in d7 '
	swap	d6              ;|
	move.w	IMM (0),d6	;| and b's sign in d6 '
	eor.l	d7,d6		;| compare the signs
	bmi	Lsubdf0	;| if the signs are different we have 
				;| to subtract
#ifndef __mcf5200__
	exg	d7,a0		;| else we add the numbers
	exg	d6,a3		;|
#else
	move.l	d7,a4
	move.l	a0,d7
	move.l	a4,a0
	move.l	d6,a4
	move.l	a3,d6
	move.l	a4,a3
#endif
	add.l	d7,d3		;|
	addx.l	d6,d2		;|
	addx.l	d5,d1		;| 
	addx.l	d4,d0           ;|

	move.l	a2,d4		;| return exponent to d4
	move.l	a0,d7		;| 
	and.l	IMM (0x80000000),d7 ;| d7 now has the sign

#ifndef __mcf5200__
	movem.l	(sp)+,a2-a3	
#else
	move.l	(sp)+,a4	
	move.l	(sp)+,a3	
	move.l	(sp)+,a2	
#endif

;| Before rounding normalize so bit #DBL_MANT_DIG is set (we will consider
;| the case of denormalized numbers in the rounding routine itself).
;| As in the addition (not in the subtraction!) we could have set 
;| one more bit we check this:
	btst	IMM (DBL_MANT_DIG+1),d0	
	beq	1f
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
	add.w	IMM (1),d4
#else
	lsr.l	IMM (1),d3
	btst	IMM (0),d2
	beq	10f
	bset	IMM (31),d3
10:	lsr.l	IMM (1),d2
	btst	IMM (0),d1
	beq	11f
	bset	IMM (31),d2
11:	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	12f
	bset	IMM (31),d1
12:	lsr.l	IMM (1),d0
	add.l	IMM (1),d4
#endif
1:
	lea	Ladddf5,a0	;| to return from rounding routine
	lea	SYM (_fpCCR),a1	;| check the rounding mode
#ifdef __mcf5200__
	clr.l	d6
#endif
	move.w	6(a1),d6	;| rounding mode in d6
	beq	Lroundtonearest
#ifndef __mcf5200__
	cmp.w	IMM (ROUND_TO_PLUS),d6
#else
	cmp.l	IMM (ROUND_TO_PLUS),d6
#endif
	bhi	Lroundtominus
	blt	Lroundtozero
	bra	Lroundtoplus
Ladddf5:
;| Put back the exponent and check for overflow
#ifndef __mcf5200__
	cmp.w	IMM (0x7ff),d4	;| is the exponent big?
#else
	cmp.l	IMM (0x7ff),d4	;| is the exponent big?
#endif
	bge	1f
	bclr	IMM (DBL_MANT_DIG-1),d0
#ifndef __mcf5200__
	lsl.w	IMM (4),d4	;| put exponent back into position
#else
	lsl.l	IMM (4),d4	;| put exponent back into position
#endif
	swap	d0		;| 
#ifndef __mcf5200__
	or.w	d4,d0		;|
#else
	or.l	d4,d0		;|
#endif
	swap	d0		;|
	bra	Ladddfret
1:
	move.w	IMM (ADD),d5
	bra	Ldoverflow

Lsubdf0:
;| Here we do the subtraction.
#ifndef __mcf5200__
	exg	d7,a0		;| put sign back in a0
	exg	d6,a3		;|
#else
	move.l	d7,a4
	move.l	a0,d7
	move.l	a4,a0
	move.l	d6,a4
	move.l	a3,d6
	move.l	a4,a3
#endif
	sub.l	d7,d3		;|
	subx.l	d6,d2		;|
	subx.l	d5,d1		;|
	subx.l	d4,d0		;|
	beq	Ladddfret1	;| if zero just exit
	bpl	1f		;| if positive skip the following
	move.l	a0,d7		;|
	bchg	IMM (31),d7	;| change sign bit in d7
	move.l	d7,a0		;|
	neg.l	d3		;|
	negx.l	d2		;|
	negx.l	d1              ;| and negate result
	negx.l	d0              ;|
1:	
	move.l	a2,d4		;| return exponent to d4
	move.l	a0,d7
	and.l	IMM (0x80000000),d7 ;| isolate sign bit
#ifndef __mcf5200__
	movem.l	(sp)+,a2-a3	;|
#else
	move.l	(sp)+,a4
	move.l	(sp)+,a3
	move.l	(sp)+,a2
#endif

;| Before rounding normalize so bit #DBL_MANT_DIG is set (we will consider
;| the case of denormalized numbers in the rounding routine itself).
;| As in the addition (not in the subtraction!) we could have set 
;| one more bit we check this:
	btst	IMM (DBL_MANT_DIG+1),d0	
	beq	1f
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
	add.w	IMM (1),d4
#else
	lsr.l	IMM (1),d3
	btst	IMM (0),d2
	beq	10f
	bset	IMM (31),d3
10:	lsr.l	IMM (1),d2
	btst	IMM (0),d1
	beq	11f
	bset	IMM (31),d2
11:	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	12f
	bset	IMM (31),d1
12:	lsr.l	IMM (1),d0
	add.l	IMM (1),d4
#endif
1:
	lea	Lsubdf1,a0	;| to return from rounding routine
	lea	SYM (_fpCCR),a1	;| check the rounding mode
#ifdef __mcf5200__
	clr.l	d6
#endif
	move.w	6(a1),d6	;| rounding mode in d6
	beq	Lroundtonearest
#ifndef __mcf5200__
	cmp.w	IMM (ROUND_TO_PLUS),d6
#else
	cmp.l	IMM (ROUND_TO_PLUS),d6
#endif
	bhi	Lroundtominus
	blt	Lroundtozero
	bra	Lroundtoplus
Lsubdf1:
;| Put back the exponent and sign (we don't have overflow). '
	bclr	IMM (DBL_MANT_DIG-1),d0	
#ifndef __mcf5200__
	lsl.w	IMM (4),d4	;| put exponent back into position
#else
	lsl.l	IMM (4),d4	;| put exponent back into position
#endif
	swap	d0		;| 
#ifndef __mcf5200__
	or.w	d4,d0		;|
#else
	or.l	d4,d0		;|
#endif
	swap	d0		;|
	bra	Ladddfret

;| If one of the numbers was too small (difference of exponents >= 
;| DBL_MANT_DIG+1) we return the other (and now we don't have to '
;| check for finiteness or zero).
Ladddfasmall:
#ifndef __mcf5200__
	movem.l	(sp)+,a2-a3	
#else
	move.l	(sp)+,a4
	move.l	(sp)+,a3
	move.l	(sp)+,a2
#endif
	move.l	16(a6),d0
	move.l	20(a6),d1
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| restore data registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| and return
	rts

Ladddfbsmall:
#ifndef __mcf5200__
	movem.l	(sp)+,a2-a3	
#else
	move.l	(sp)+,a4	
	move.l	(sp)+,a3	
	move.l	(sp)+,a2	
#endif
	move.l	8(a6),d0
	move.l	12(a6),d1
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| restore data registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| and return
	rts

Ladddfaden:
	move.l	d7,d4		;| d7 contains 0x00200000
	bra	Ladddf1

Ladddfbden:
	move.l	d7,d5           ;| d7 contains 0x00200000
	not.l	d6
	bra	Ladddf2

Ladddfb:
;| Return b (if a is zero)
	move.l	d2,d0
	move.l	d3,d1
	bra	1f
Ladddfa:
	move.l	8(a6),d0
	move.l	12(a6),d1
1:
	move.w	IMM (ADD),d5
;| Check for NaN and +/-INFINITY.
	move.l	d0,d7         		;|
	and.l	IMM (0x80000000),d7	;|
	bclr	IMM (31),d0		;|
	cmp.l	IMM (0x7ff00000),d0	;|
	bge	2f			;|
	move.l	d0,d0           	;| check for zero, since we don't  '
	bne	Ladddfret		;| want to return -0 by mistake
	bclr	IMM (31),d7		;|
	bra	Ladddfret		;|
2:
	and.l	IMM (0x000fffff),d0	;| check for NaN (nonzero fraction)
	or.l	d1,d0			;|
	bne	Ldinop         	;|
	bra	Ldinfty		;|
	
Ladddfret1:
#ifndef __mcf5200__
	movem.l	(sp)+,a2-a3	;| restore regs and exit
#else
	move.l	(sp)+,a4
	move.l	(sp)+,a3
	move.l	(sp)+,a2
#endif

Ladddfret:
;| Normal exit.
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
	or.l	d7,d0		;| put sign bit back
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts

Ladddfretden:
;| Return a denormalized number.
#ifndef __mcf5200__
	lsr.l	IMM (1),d0	;| shift right once more
	roxr.l	IMM (1),d1	;|
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
#endif
	bra	Ladddfret

Ladddfnf:
	move.w	IMM (ADD),d5
;| This could be faster but it is not worth the effort, since it is not
;| executed very often. We sacrifice speed for clarity here.
	move.l	8(a6),d0	;| get the numbers back (remember that we
	move.l	12(a6),d1	;| did some processing already)
	move.l	16(a6),d2	;| 
	move.l	20(a6),d3	;| 
	move.l	IMM (0x7ff00000),d4 ;| useful constant (INFINITY)
	move.l	d0,d7		;| save sign bits
	move.l	d2,d6		;| 
	bclr	IMM (31),d0	;| clear sign bits
	bclr	IMM (31),d2	;| 
;| We know that one of them is either NaN of +/-INFINITY
;| Check for NaN (if either one is NaN return NaN)
	cmp.l	d4,d0		;| check first a (d0)
	bhi	Ldinop		;| if d0 > 0x7ff00000 or equal and
	bne	2f
	tst.l	d1		;| d1 > 0, a is NaN
	bne	Ldinop		;| 
2:	cmp.l	d4,d2		;| check now b (d1)
	bhi	Ldinop		;| 
	bne	3f
	tst.l	d3		;| 
	bne	Ldinop		;| 
3:
;| Now comes the check for +/-INFINITY. We know that both are (maybe not
;| finite) numbers, but we have to check if both are infinite whether we
;| are adding or subtracting them.
	eor.l	d7,d6		;| to check sign bits
	bmi	1f
	and.l	IMM (0x80000000),d7 ;| get (common) sign bit
	bra	Ldinfty
1:
;| We know one (or both) are infinite, so we test for equality between the
;| two numbers (if they are equal they have to be infinite both, so we
;| return NaN).
	cmp.l	d2,d0		;| are both infinite?
	bne	1f		;| if d0 <> d2 they are not equal
	cmp.l	d3,d1		;| if d0 == d2 test d3 and d1
	beq	Ldinop		;| if equal return NaN
1:	
	and.l	IMM (0x80000000),d7 ;| get a's sign bit '
	cmp.l	d4,d0		;| test now for infinity
	beq	Ldinfty	;| if a is INFINITY return with this sign
	bchg	IMM (31),d7	;| else we know b is INFINITY and has
	bra	Ldinfty	;| the opposite sign

;|=============================================================================
;|                              __muldf3
;|=============================================================================

;| double __muldf3(double, double);
SYM (__muldf3):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.l	8(a6),d0		;| get a into d0-d1
	move.l	12(a6),d1		;| 
	move.l	16(a6),d2		;| and b into d2-d3
	move.l	20(a6),d3		;|
	move.l	d0,d7			;| d7 will hold the sign of the product
	eor.l	d2,d7			;|
	and.l	IMM (0x80000000),d7	;|
	move.l	d7,a0			;| save sign bit into a0 
	move.l	IMM (0x7ff00000),d7	;| useful constant (+INFINITY)
	move.l	d7,d6			;| another (mask for fraction)
	not.l	d6			;|
	bclr	IMM (31),d0		;| get rid of a's sign bit '
	move.l	d0,d4			;| 
	or.l	d1,d4			;| 
	beq	Lmuldfa0		;| branch if a is zero
	move.l	d0,d4			;|
	bclr	IMM (31),d2		;| get rid of b's sign bit '
	move.l	d2,d5			;|
	or.l	d3,d5			;| 
	beq	Lmuldfb0		;| branch if b is zero
	move.l	d2,d5			;| 
	cmp.l	d7,d0			;| is a big?
	bhi	Lmuldfinop		;| if a is NaN return NaN
	beq	Lmuldfanf		;| we still have to check d1 and b ...
	cmp.l	d7,d2			;| now compare b with INFINITY
	bhi	Lmuldfinop		;| is b NaN?
	beq	Lmuldfbnf 		;| we still have to check d3 ...
;| Here we have both numbers finite and nonzero (and with no sign bit).
;| Now we get the exponents into d4 and d5.
	and.l	d7,d4			;| isolate exponent in d4
	beq	Lmuldfaden		;| if exponent zero, have denormalized
	and.l	d6,d0			;| isolate fraction
	or.l	IMM (0x00100000),d0	;| and put hidden bit back
	swap	d4			;| I like exponents in the first byte
#ifndef __mcf5200__
	lsr.w	IMM (4),d4		;| 
#else
	lsr.l	IMM (4),d4		;| 
#endif
Lmuldf1:			
	and.l	d7,d5			;|
	beq	Lmuldfbden		;|
	and.l	d6,d2			;|
	or.l	IMM (0x00100000),d2	;| and put hidden bit back
	swap	d5			;|
#ifndef __mcf5200__
	lsr.w	IMM (4),d5		;|
#else
	lsr.l	IMM (4),d5		;|
#endif
Lmuldf2:				;|
#ifndef __mcf5200__
	add.w	d5,d4			;| add exponents
	sub.w	IMM (D_BIAS+1),d4	;| and subtract bias (plus one)
#else
	add.l	d5,d4			;| add exponents
	sub.l	IMM (D_BIAS+1),d4	;| and subtract bias (plus one)
#endif

;| We are now ready to do the multiplication. The situation is as follows:
;| both a and b have bit 52 ( bit 20 of d0 and d2) set (even if they were 
;| denormalized to start with!), which means that in the product bit 104 
;| (which will correspond to bit 8 of the fourth long) is set.

;| Here we have to do the product.
;| To do it we have to juggle the registers back and forth, as there are not
;| enough to keep everything in them. So we use the address registers to keep
;| some intermediate data.

#ifndef __mcf5200__
	movem.l	a2-a3,-(sp)	;| save a2 and a3 for temporary use
#else
	move.l	a2,-(sp)
	move.l	a3,-(sp)
	move.l	a4,-(sp)
#endif
	move.l	IMM (0),a2	;| a2 is a null register
	move.l	d4,a3		;| and a3 will preserve the exponent

;| First, shift d2-d3 so bit 20 becomes bit 31:
#ifndef __mcf5200__
	ror.l	IMM (5),d2	;| rotate d2 5 places right
	swap	d2		;| and swap it
	ror.l	IMM (5),d3	;| do the same thing with d3
	swap	d3		;|
	move.w	d3,d6		;| get the rightmost 11 bits of d3
	and.w	IMM (0x07ff),d6	;|
	or.w	d6,d2		;| and put them into d2
	and.w	IMM (0xf800),d3	;| clear those bits in d3
#else
	moveq	IMM (11),d7	;| left shift d2 11 bits
	lsl.l	d7,d2
	move.l	d3,d6		;| get a copy of d3
	lsl.l	d7,d3		;| left shift d3 11 bits
	and.l	IMM (0xffe00000),d6 ;| get the top 11 bits of d3
	moveq	IMM (21),d7	;| right shift them 21 bits
	lsr.l	d7,d6
	or.l	d6,d2		;| stick them at the end of d2
#endif

	move.l	d2,d6		;| move b into d6-d7
	move.l	d3,d7           ;| move a into d4-d5
	move.l	d0,d4           ;| and clear d0-d1-d2-d3 (to put result)
	move.l	d1,d5           ;|
	move.l	IMM (0),d3	;|
	move.l	d3,d2           ;|
	move.l	d3,d1           ;|
	move.l	d3,d0	        ;|

;| We use a1 as counter:	
	move.l	IMM (DBL_MANT_DIG-1),a1		
#ifndef __mcf5200__
	exg	d7,a1
#else
	move.l	d7,a4
	move.l	a1,d7
	move.l	a4,a1
#endif

1:
#ifndef __mcf5200__
	exg	d7,a1		;| put counter back in a1
#else
	move.l	d7,a4
	move.l	a1,d7
	move.l	a4,a1
#endif
	add.l	d3,d3		;| shift sum once left
	addx.l	d2,d2           ;|
	addx.l	d1,d1           ;|
	addx.l	d0,d0           ;|
	add.l	d7,d7		;|
	addx.l	d6,d6		;|
	bcc	2f		;| if bit clear skip the following
#ifndef __mcf5200__
	exg	d7,a2		;|
#else
	move.l	d7,a4
	move.l	a2,d7
	move.l	a4,a2
#endif
	add.l	d5,d3		;| else add a to the sum
	addx.l	d4,d2		;|
	addx.l	d7,d1		;|
	addx.l	d7,d0		;|
#ifndef __mcf5200__
	exg	d7,a2		;| 
#else
	move.l	d7,a4
	move.l	a2,d7
	move.l	a4,a2
#endif
2:
#ifndef __mcf5200__
	exg	d7,a1		;| put counter in d7
	dbf	d7,1b		;| decrement and branch
#else
	move.l	d7,a4
	move.l	a1,d7
	move.l	a4,a1
	subq.l	IMM (1),d7
	bpl	1b
#endif

	move.l	a3,d4		;| restore exponent
#ifndef __mcf5200__
	movem.l	(sp)+,a2-a3
#else
	move.l	(sp)+,a4
	move.l	(sp)+,a3
	move.l	(sp)+,a2
#endif

;| Now we have the product in d0-d1-d2-d3, with bit 8 of d0 set. The 
;| first thing to do now is to normalize it so bit 8 becomes bit 
;| DBL_MANT_DIG-32 (to do the rounding); later we will shift right.
	swap	d0
	swap	d1
	move.w	d1,d0
	swap	d2
	move.w	d2,d1
	swap	d3
	move.w	d3,d2
	move.w	IMM (0),d3
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
#else
	moveq	IMM (29),d6
	lsr.l	IMM (3),d3
	move.l	d2,d7
	lsl.l	d6,d7
	or.l	d7,d3
	lsr.l	IMM (3),d2
	move.l	d1,d7
	lsl.l	d6,d7
	or.l	d7,d2
	lsr.l	IMM (3),d1
	move.l	d0,d7
	lsl.l	d6,d7
	or.l	d7,d1
	lsr.l	IMM (3),d0
#endif
	
;| Now round, check for over- and underflow, and exit.
	move.l	a0,d7		;| get sign bit back into d7
	move.w	IMM (MULTIPLY),d5

	btst	IMM (DBL_MANT_DIG+1-32),d0
	beq	Lroundexit
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	add.w	IMM (1),d4
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
	add.l	IMM (1),d4
#endif
	bra	Lroundexit

Lmuldfinop:
	move.w	IMM (MULTIPLY),d5
	bra	Ldinop

Lmuldfbnf:
	move.w	IMM (MULTIPLY),d5
	move.l	a0,d7		;| get sign bit back into d7
	tst.l	d3		;| we know d2 == 0x7ff00000, so check d3
	bne	Ldinop		;| if d3 <> 0 b is NaN
	bra	Ldoverflow	;| else we have overflow (since a is finite)

Lmuldfanf:
	move.w	IMM (MULTIPLY),d5
	move.l	a0,d7		;| get sign bit back into d7
	tst.l	d1		;| we know d0 == 0x7ff00000, so check d1
	bne	Ldinop		;| if d1 <> 0 a is NaN
	bra	Ldoverflow	;| else signal overflow

;| If either number is zero return zero, unless the other is +/-INFINITY or
;| NaN, in which case we return NaN.
Lmuldfb0:
	move.w	IMM (MULTIPLY),d5
#ifndef __mcf5200__
	exg	d2,d0		;| put b (==0) into d0-d1
	exg	d3,d1		;| and a (with sign bit cleared) into d2-d3
#else
	move.l	d2,d7
	move.l	d0,d2
	move.l	d7,d0
	move.l	d3,d7
	move.l	d1,d3
	move.l	d7,d1
#endif
	bra	1f
Lmuldfa0:
	move.l	16(a6),d2	;| put b into d2-d3 again
	move.l	20(a6),d3	;|
	bclr	IMM (31),d2	;| clear sign bit
1:	cmp.l	IMM (0x7ff00000),d2 ;| check for non-finiteness
	bge	Ldinop		;| in case NaN or +/-INFINITY return NaN
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts

;| If a number is denormalized we put an exponent of 1 but do not put the 
;| hidden bit back into the fraction; instead we shift left until bit 21
;| (the hidden bit) is set, adjusting the exponent accordingly. We do this
;| to ensure that the product of the fractions is close to 1.
Lmuldfaden:
	move.l	IMM (1),d4
	and.l	d6,d0
1:	add.l	d1,d1           ;| shift a left until bit 20 is set
	addx.l	d0,d0		;|
#ifndef __mcf5200__
	sub.w	IMM (1),d4	;| and adjust exponent
#else
	sub.l	IMM (1),d4	;| and adjust exponent
#endif
	btst	IMM (20),d0	;|
	bne	Lmuldf1        ;|
	bra	1b

Lmuldfbden:
	move.l	IMM (1),d5
	and.l	d6,d2
1:	add.l	d3,d3		;| shift b left until bit 20 is set
	addx.l	d2,d2		;|
#ifndef __mcf5200__
	sub.w	IMM (1),d5	;| and adjust exponent
#else
	subq.l	IMM (1),d5	;| and adjust exponent
#endif
	btst	IMM (20),d2	;|
	bne	Lmuldf2	;|
	bra	1b


;|=============================================================================
;|                              __divdf3
;|=============================================================================

;| double __divdf3(double, double);
SYM (__divdf3):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.l	8(a6),d0	;| get a into d0-d1
	move.l	12(a6),d1	;| 
	move.l	16(a6),d2	;| and b into d2-d3
	move.l	20(a6),d3	;|
	move.l	d0,d7		;| d7 will hold the sign of the result
	eor.l	d2,d7		;|
	and.l	IMM (0x80000000),d7
	move.l	d7,a0		;| save sign into a0
	move.l	IMM (0x7ff00000),d7 ;| useful constant (+INFINITY)
	move.l	d7,d6		;| another (mask for fraction)
	not.l	d6		;|
	bclr	IMM (31),d0	;| get rid of a's sign bit '
	move.l	d0,d4		;|
	or.l	d1,d4		;|
	beq	Ldivdfa0	;| branch if a is zero
	move.l	d0,d4		;|
	bclr	IMM (31),d2	;| get rid of b's sign bit '
	move.l	d2,d5		;|
	or.l	d3,d5		;|
	beq	Ldivdfb0	;| branch if b is zero
	move.l	d2,d5
	cmp.l	d7,d0		;| is a big?
	bhi	Ldivdfinop	;| if a is NaN return NaN
	beq	Ldivdfanf	;| if d0 == 0x7ff00000 we check d1
	cmp.l	d7,d2		;| now compare b with INFINITY 
	bhi	Ldivdfinop	;| if b is NaN return NaN
	beq	Ldivdfbnf	;| if d2 == 0x7ff00000 we check d3
;| Here we have both numbers finite and nonzero (and with no sign bit).
;| Now we get the exponents into d4 and d5 and normalize the numbers to
;| ensure that the ratio of the fractions is around 1. We do this by
;| making sure that both numbers have bit #DBL_MANT_DIG-32-1 (hidden bit)
;| set, even if they were denormalized to start with.
;| Thus, the result will satisfy: 2 > result > 1/2.
	and.l	d7,d4		;| and isolate exponent in d4
	beq	Ldivdfaden	;| if exponent is zero we have a denormalized
	and.l	d6,d0		;| and isolate fraction
	or.l	IMM (0x00100000),d0 ;| and put hidden bit back
	swap	d4		;| I like exponents in the first byte
#ifndef __mcf5200__
	lsr.w	IMM (4),d4	;| 
#else
	lsr.l	IMM (4),d4	;| 
#endif
Ldivdf1:			;| 
	and.l	d7,d5		;|
	beq	Ldivdfbden	;|
	and.l	d6,d2		;|
	or.l	IMM (0x00100000),d2
	swap	d5		;|
#ifndef __mcf5200__
	lsr.w	IMM (4),d5	;|
#else
	lsr.l	IMM (4),d5	;|
#endif
Ldivdf2:			;|
#ifndef __mcf5200__
	sub.w	d5,d4		;| subtract exponents
	add.w	IMM (D_BIAS),d4	;| and add bias
#else
	sub.l	d5,d4		;| subtract exponents
	add.l	IMM (D_BIAS),d4	;| and add bias
#endif

;| We are now ready to do the division. We have prepared things in such a way
;| that the ratio of the fractions will be less than 2 but greater than 1/2.
;| At this point the registers in use are:
;| d0-d1	hold a (first operand, bit DBL_MANT_DIG-32=0, bit 
;| DBL_MANT_DIG-1-32=1)
;| d2-d3	hold b (second operand, bit DBL_MANT_DIG-32=1)
;| d4	holds the difference of the exponents, corrected by the bias
;| a0	holds the sign of the ratio

;| To do the rounding correctly we need to keep information about the
;| nonsignificant bits. One way to do this would be to do the division
;| using four registers; another is to use two registers (as originally
;| I did), but use a sticky bit to preserve information about the 
;| fractional part. Note that we can keep that info in a1, which is not
;| used.
	move.l	IMM (0),d6	;| d6-d7 will hold the result
	move.l	d6,d7		;| 
	move.l	IMM (0),a1	;| and a1 will hold the sticky bit

	move.l	IMM (DBL_MANT_DIG-32+1),d5	
	
1:	cmp.l	d0,d2		;| is a < b?
	bhi	3f		;| if b > a skip the following
	beq	4f		;| if d0==d2 check d1 and d3
2:	sub.l	d3,d1		;| 
	subx.l	d2,d0		;| a <-- a - b
	bset	d5,d6		;| set the corresponding bit in d6
3:	add.l	d1,d1		;| shift a by 1
	addx.l	d0,d0		;|
#ifndef __mcf5200__
	dbra	d5,1b		;| and branch back
#else
	subq.l	IMM (1),d5
	bpl	1b
#endif
	bra	5f			
4:	cmp.l	d1,d3		;| here d0==d2, so check d1 and d3
	bhi	3b		;| if d1 > d2 skip the subtraction
	bra	2b		;| else go do it
5:
;| Here we have to start setting the bits in the second long.
	move.l	IMM (31),d5	;| again d5 is counter

1:	cmp.l	d0,d2		;| is a < b?
	bhi	3f		;| if b > a skip the following
	beq	4f		;| if d0==d2 check d1 and d3
2:	sub.l	d3,d1		;| 
	subx.l	d2,d0		;| a <-- a - b
	bset	d5,d7		;| set the corresponding bit in d7
3:	add.l	d1,d1		;| shift a by 1
	addx.l	d0,d0		;|
#ifndef __mcf5200__
	dbra	d5,1b		;| and branch back
#else
	subq.l	IMM (1),d5
	bpl	1b
#endif
	bra	5f			
4:	cmp.l	d1,d3		;| here d0==d2, so check d1 and d3
	bhi	3b		;| if d1 > d2 skip the subtraction
	bra	2b		;| else go do it
5:
;| Now go ahead checking until we hit a one, which we store in d2.
	move.l	IMM (DBL_MANT_DIG),d5
1:	cmp.l	d2,d0		;| is a < b?
	bhi	4f		;| if b < a, exit
	beq	3f		;| if d0==d2 check d1 and d3
2:	add.l	d1,d1		;| shift a by 1
	addx.l	d0,d0		;|
#ifndef __mcf5200__
	dbra	d5,1b		;| and branch back
#else
	subq.l	IMM (1),d5
	bpl	1b
#endif
	move.l	IMM (0),d2	;| here no sticky bit was found
	move.l	d2,d3
	bra	5f			
3:	cmp.l	d1,d3		;| here d0==d2, so check d1 and d3
	bhi	2b		;| if d1 > d2 go back
4:
;| Here put the sticky bit in d2-d3 (in the position which actually corresponds
;| to it; if you don't do this the algorithm loses in some cases). '
	move.l	IMM (0),d2
	move.l	d2,d3
#ifndef __mcf5200__
	sub.w	IMM (DBL_MANT_DIG),d5
	add.w	IMM (63),d5
	cmp.w	IMM (31),d5
#else
	sub.l	IMM (DBL_MANT_DIG),d5
	add.l	IMM (63),d5
	cmp.l	IMM (31),d5
#endif
	bhi	2f
1:	bset	d5,d3
	bra	5f
#ifndef __mcf5200__
	sub.w	IMM (32),d5
#else
	sub.l	IMM (32),d5
#endif
2:	bset	d5,d2
5:
;| Finally we are finished! Move the longs in the address registers to
;| their final destination:
	move.l	d6,d0
	move.l	d7,d1
	move.l	IMM (0),d3

;| Here we have finished the division, with the result in d0-d1-d2-d3, with
;| 2^21 <= d6 < 2^23. Thus bit 23 is not set, but bit 22 could be set.
;| If it is not, then definitely bit 21 is set. Normalize so bit 22 is
;| not set:
	btst	IMM (DBL_MANT_DIG-32+1),d0
	beq	1f
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	roxr.l	IMM (1),d2
	roxr.l	IMM (1),d3
	add.w	IMM (1),d4
#else
	lsr.l	IMM (1),d3
	btst	IMM (0),d2
	beq	10f
	bset	IMM (31),d3
10:	lsr.l	IMM (1),d2
	btst	IMM (0),d1
	beq	11f
	bset	IMM (31),d2
11:	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	12f
	bset	IMM (31),d1
12:	lsr.l	IMM (1),d0
	add.l	IMM (1),d4
#endif
1:
;| Now round, check for over- and underflow, and exit.
	move.l	a0,d7		;| restore sign bit to d7
	move.w	IMM (DIVIDE),d5
	bra	Lroundexit

Ldivdfinop:
	move.w	IMM (DIVIDE),d5
	bra	Ldinop

Ldivdfa0:
;| If a is zero check to see whether b is zero also. In that case return
;| NaN; then check if b is NaN, and return NaN also in that case. Else
;| return zero.
	move.w	IMM (DIVIDE),d5
	bclr	IMM (31),d2	;|
	move.l	d2,d4		;| 
	or.l	d3,d4		;| 
	beq	Ldinop		;| if b is also zero return NaN
	cmp.l	IMM (0x7ff00000),d2 ;| check for NaN
	bhi	Ldinop		;| 
	blt	1f		;|
	tst.l	d3		;|
	bne	Ldinop		;|
1:	move.l	IMM (0),d0	;| else return zero
	move.l	d0,d1		;| 
	lea	SYM (_fpCCR),a0	;| clear exception flags
	move.w	IMM (0),(a0)	;|
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| 
#else
	movem.l	(sp),d2-d7	;| 
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| 
	rts			;| 	

Ldivdfb0:
	move.w	IMM (DIVIDE),d5
;| If we got here a is not zero. Check if a is NaN; in that case return NaN,
;| else return +/-INFINITY. Remember that a is in d0 with the sign bit 
;| cleared already.
	move.l	a0,d7		;| put a's sign bit back in d7 '
	cmp.l	IMM (0x7ff00000),d0 ;| compare d0 with INFINITY
	bhi	Ldinop		;| if larger it is NaN
	tst.l	d1		;| 
	bne	Ldinop		;| 
	bra	Lddiv0	;| else signal DIVIDE_BY_ZERO

Ldivdfbnf:
	move.w	IMM (DIVIDE),d5
;| If d2 == 0x7ff00000 we have to check d3.
	tst.l	d3		;|
	bne	Ldinop		;| if d3 <> 0, b is NaN
	bra	Ldunderflow	;| else b is +/-INFINITY, so signal underflow

Ldivdfanf:
	move.w	IMM (DIVIDE),d5
;| If d0 == 0x7ff00000 we have to check d1.
	tst.l	d1		;|
	bne	Ldinop		;| if d1 <> 0, a is NaN
;| If a is INFINITY we have to check b
	cmp.l	d7,d2		;| compare b with INFINITY 
	bge	Ldinop		;| if b is NaN or INFINITY return NaN
	tst.l	d3		;|
	bne	Ldinop		;| 
	bra	Ldoverflow	;| else return overflow

;| If a number is denormalized we put an exponent of 1 but do not put the 
;| bit back into the fraction.
Ldivdfaden:
	move.l	IMM (1),d4
	and.l	d6,d0
1:	add.l	d1,d1		;| shift a left until bit 20 is set
	addx.l	d0,d0
#ifndef __mcf5200__
	sub.w	IMM (1),d4	;| and adjust exponent
#else
	sub.l	IMM (1),d4	;| and adjust exponent
#endif
	btst	IMM (DBL_MANT_DIG-32-1),d0
	bne	Ldivdf1
	bra	1b

Ldivdfbden:
	move.l	IMM (1),d5
	and.l	d6,d2
1:	add.l	d3,d3		;| shift b left until bit 20 is set
	addx.l	d2,d2
#ifndef __mcf5200__
	sub.w	IMM (1),d5	;| and adjust exponent
#else
	subq.l	IMM (1),d5	;| and adjust exponent
#endif
	btst	IMM (DBL_MANT_DIG-32-1),d2
	bne	Ldivdf2
	bra	1b

Lroundexit:
;| This is a common exit point for __muldf3 and __divdf3. When they enter
;| this point the sign of the result is in d7, the result in d0-d1, normalized
;| so that 2^21 <= d0 < 2^22, and the exponent is in the lower byte of d4.

;| First check for underlow in the exponent:
#ifndef __mcf5200__
	cmp.w	IMM (-DBL_MANT_DIG-1),d4		
#else
	cmp.l	IMM (-DBL_MANT_DIG-1),d4		
#endif
	blt	Ldunderflow	
;| It could happen that the exponent is less than 1, in which case the 
;| number is denormalized. In this case we shift right and adjust the 
;| exponent until it becomes 1 or the fraction is zero (in the latter case 
;| we signal underflow and return zero).
	move.l	d7,a0		;|
	move.l	IMM (0),d6	;| use d6-d7 to collect bits flushed right
	move.l	d6,d7		;| use d6-d7 to collect bits flushed right
#ifndef __mcf5200__
	cmp.w	IMM (1),d4	;| if the exponent is less than 1 we 
#else
	cmp.l	IMM (1),d4	;| if the exponent is less than 1 we 
#endif
	bge	2f		;| have to shift right (denormalize)
1:
#ifndef __mcf5200__
	add.w	IMM (1),d4	;| adjust the exponent
	lsr.l	IMM (1),d0	;| shift right once 
	roxr.l	IMM (1),d1	;|
	roxr.l	IMM (1),d2	;|
	roxr.l	IMM (1),d3	;|
	roxr.l	IMM (1),d6	;| 
	roxr.l	IMM (1),d7	;|
	cmp.w	IMM (1),d4	;| is the exponent 1 already?
#else
	add.l	IMM (1),d4	;| adjust the exponent
	lsr.l	IMM (1),d7
	btst	IMM (0),d6
	beq	13f
	bset	IMM (31),d7
13:	lsr.l	IMM (1),d6
	btst	IMM (0),d3
	beq	14f
	bset	IMM (31),d6
14:	lsr.l	IMM (1),d3
	btst	IMM (0),d2
	beq	10f
	bset	IMM (31),d3
10:	lsr.l	IMM (1),d2
	btst	IMM (0),d1
	beq	11f
	bset	IMM (31),d2
11:	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	12f
	bset	IMM (31),d1
12:	lsr.l	IMM (1),d0
	cmp.l	IMM (1),d4	;| is the exponent 1 already?
#endif
	beq	2f		;| if not loop back
	bra	1b              ;|
	bra	Ldunderflow	;| safety check, shouldn't execute '
2:	or.l	d6,d2		;| this is a trick so we don't lose  '
	or.l	d7,d3		;| the bits which were flushed right
	move.l	a0,d7		;| get back sign bit into d7
;| Now call the rounding routine (which takes care of denormalized numbers):
	lea	Lround0,a0	;| to return from rounding routine
	lea	SYM (_fpCCR),a1	;| check the rounding mode
#ifdef __mcf5200__
	clr.l	d6
#endif
	move.w	6(a1),d6	;| rounding mode in d6
	beq	Lroundtonearest
#ifndef __mcf5200__
	cmp.w	IMM (ROUND_TO_PLUS),d6
#else
	cmp.l	IMM (ROUND_TO_PLUS),d6
#endif
	bhi	Lroundtominus
	blt	Lroundtozero
	bra	Lroundtoplus
Lround0:
;| Here we have a correctly rounded result (either normalized or denormalized).

;| Here we should have either a normalized number or a denormalized one, and
;| the exponent is necessarily larger or equal to 1 (so we don't have to  '
;| check again for underflow!). We have to check for overflow or for a 
;| denormalized number (which also signals underflow).
;| Check for overflow (i.e., exponent >= 0x7ff).
#ifndef __mcf5200__
	cmp.w	IMM (0x07ff),d4
#else
	cmp.l	IMM (0x07ff),d4
#endif
	bge	Ldoverflow
;| Now check for a denormalized number (exponent==0):
	move.w	d4,d4
	beq	Ldden
1:
;| Put back the exponents and sign and return.
#ifndef __mcf5200__
	lsl.w	IMM (4),d4	;| exponent back to fourth byte
#else
	lsl.l	IMM (4),d4	;| exponent back to fourth byte
#endif
	bclr	IMM (DBL_MANT_DIG-32-1),d0
	swap	d0		;| and put back exponent
#ifndef __mcf5200__
	or.w	d4,d0		;| 
#else
	or.l	d4,d0		;| 
#endif
	swap	d0		;|
	or.l	d7,d0		;| and sign also

	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts

;|=============================================================================
;|                              __negdf2
;|=============================================================================

;| double __negdf2(double, double);
SYM (__negdf2):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.w	IMM (NEGATE),d5
	move.l	8(a6),d0	;| get number to negate in d0-d1
	move.l	12(a6),d1	;|
	bchg	IMM (31),d0	;| negate
	move.l	d0,d2		;| make a positive copy (for the tests)
	bclr	IMM (31),d2	;|
	move.l	d2,d4		;| check for zero
	or.l	d1,d4		;|
	beq	2f		;| if zero (either sign) return +zero
	cmp.l	IMM (0x7ff00000),d2 ;| compare to +INFINITY
	blt	1f		;| if finite, return
	bhi	Ldinop		;| if larger (fraction not zero) is NaN
	tst.l	d1		;| if d2 == 0x7ff00000 check d1
	bne	Ldinop		;|
	move.l	d0,d7		;| else get sign and return INFINITY
	and.l	IMM (0x80000000),d7
	bra	Ldinfty		
1:	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts
2:	bclr	IMM (31),d0
	bra	1b

;|=============================================================================
;|                              __cmpdf2
;|=============================================================================

GREATER =  1
LESS    = -1
EQUAL   =  0

;| int __cmpdf2(double, double);
SYM (__cmpdf2):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp) 	;| save registers
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.w	IMM (COMPARE),d5
	move.l	8(a6),d0	;| get first operand
	move.l	12(a6),d1	;|
	move.l	16(a6),d2	;| get second operand
	move.l	20(a6),d3	;|
;| First check if a and/or b are (+/-) zero and in that case clear
;| the sign bit.
	move.l	d0,d6		;| copy signs into d6 (a) and d7(b)
	bclr	IMM (31),d0	;| and clear signs in d0 and d2
	move.l	d2,d7		;|
	bclr	IMM (31),d2	;|
	cmp.l	IMM (0x7fff0000),d0 ;| check for a == NaN
	bhi	Ldinop		;| if d0 > 0x7ff00000, a is NaN
	beq	Lcmpdfanf	;| if equal can be INFINITY, so check d1
	move.l	d0,d4		;| copy into d4 to test for zero
	or.l	d1,d4		;|
	beq	Lcmpdfa0	;|
Lcmpdf0:
	cmp.l	IMM (0x7fff0000),d2 ;| check for b == NaN
	bhi	Ldinop		;| if d2 > 0x7ff00000, b is NaN
	beq	Lcmpdfbnf	;| if equal can be INFINITY, so check d3
	move.l	d2,d4		;|
	or.l	d3,d4		;|
	beq	Lcmpdfb0	;|
Lcmpdf1:
;| Check the signs
	eor.l	d6,d7
	bpl	1f
;| If the signs are not equal check if a >= 0
	tst.l	d6
	bpl	Lcmpdfagtb	;| if (a >= 0 && b < 0) => a > b
	bmi	Lcmpdfbgta	;| if (a < 0 && b >= 0) => a < b
1:
;| If the signs are equal check for < 0
	tst.l	d6
	bpl	1f
;| If both are negative exchange them
#ifndef __mcf5200__
	exg	d0,d2
	exg	d1,d3
#else
	move.l	d0,d7
	move.l	d2,d0
	move.l	d7,d2
	move.l	d1,d7
	move.l	d3,d1
	move.l	d7,d3
#endif
1:
;| Now that they are positive we just compare them as longs (does this also
;| work for denormalized numbers?).
	cmp.l	d0,d2
	bhi	Lcmpdfbgta	;| ;|b;| > ;|a;|
	bne	Lcmpdfagtb	;| ;|b;| < ;|a;|
;| If we got here d0 == d2, so we compare d1 and d3.
	cmp.l	d1,d3
	bhi	Lcmpdfbgta	;| ;|b;| > ;|a;|
	bne	Lcmpdfagtb	;| ;|b;| < ;|a;|
;| If we got here a == b.
	move.l	IMM (EQUAL),d0
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7 	;| put back the registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts
Lcmpdfagtb:
	move.l	IMM (GREATER),d0
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7 	;| put back the registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts
Lcmpdfbgta:
	move.l	IMM (LESS),d0
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7 	;| put back the registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts

Lcmpdfa0:	
	bclr	IMM (31),d6
	bra	Lcmpdf0
Lcmpdfb0:
	bclr	IMM (31),d7
	bra	Lcmpdf1

Lcmpdfanf:
	tst.l	d1
	bne	Ldinop
	bra	Lcmpdf0

Lcmpdfbnf:
	tst.l	d3
	bne	Ldinop
	bra	Lcmpdf1

;|=============================================================================
;|                           rounding routines
;|=============================================================================

;| The rounding routines expect the number to be normalized in registers
;| d0-d1-d2-d3, with the exponent in register d4. They assume that the 
;| exponent is larger or equal to 1. They return a properly normalized number
;| if possible, and a denormalized number otherwise. The exponent is returned
;| in d4.

Lroundtonearest:
;| We now normalize as suggested by D. Knuth ("Seminumerical Algorithms"):
;| Here we assume that the exponent is not too small (this should be checked
;| before entering the rounding routine), but the number could be denormalized.

;| Check for denormalized numbers:
1:	btst	IMM (DBL_MANT_DIG-32),d0
	bne	2f		;| if set the number is normalized
;| Normalize shifting left until bit #DBL_MANT_DIG-32 is set or the exponent 
;| is one (remember that a denormalized number corresponds to an 
;| exponent of -D_BIAS+1).
#ifndef __mcf5200__
	cmp.w	IMM (1),d4	;| remember that the exponent is at least one
#else
	cmp.l	IMM (1),d4	;| remember that the exponent is at least one
#endif
 	beq	2f		;| an exponent of one means denormalized
	add.l	d3,d3		;| else shift and adjust the exponent
	addx.l	d2,d2		;|
	addx.l	d1,d1		;|
	addx.l	d0,d0		;|
#ifndef __mcf5200__
	dbra	d4,1b		;|
#else
	subq.l	IMM (1),d4
	bpl	1b
#endif
2:
;| Now round: we do it as follows: after the shifting we can write the
;| fraction part as f + delta, where 1 < f < 2^25, and 0 <= delta <= 2.
;| If delta < 1, do nothing. If delta > 1, add 1 to f. 
;| If delta == 1, we make sure the rounded number will be even (odd?) 
;| (after shifting).
	btst	IMM (0),d1	;| is delta < 1?
	beq	2f		;| if so, do not do anything
	or.l	d2,d3		;| is delta == 1?
	bne	1f		;| if so round to even
	move.l	d1,d3		;| 
	and.l	IMM (2),d3	;| bit 1 is the last significant bit
	move.l	IMM (0),d2	;|
	add.l	d3,d1		;|
	addx.l	d2,d0		;|
	bra	2f		;| 
1:	move.l	IMM (1),d3	;| else add 1 
	move.l	IMM (0),d2	;|
	add.l	d3,d1		;|
	addx.l	d2,d0
;| Shift right once (because we used bit #DBL_MANT_DIG-32!).
2:
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1		
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
#endif

;| Now check again bit #DBL_MANT_DIG-32 (rounding could have produced a
;| 'fraction overflow' ...).
	btst	IMM (DBL_MANT_DIG-32),d0	
	beq	1f
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	add.w	IMM (1),d4
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
	add.l	IMM (1),d4
#endif
1:
;| If bit #DBL_MANT_DIG-32-1 is clear we have a denormalized number, so we 
;| have to put the exponent to zero and return a denormalized number.
	btst	IMM (DBL_MANT_DIG-32-1),d0
	beq	1f
	jmp	(a0)
1:	move.l	IMM (0),d4
	jmp	(a0)

Lroundtozero:
Lroundtoplus:
Lroundtominus:
	jmp	(a0)
#endif /* L_double */

#ifdef  L_float

	.globl	SYM (_fpCCR)
	.globl  _exception_handler

QUIET_NaN    = 0xffffffff
SIGNL_NaN    = 0x7f800001
INFINITY     = 0x7f800000

F_MAX_EXP      = 0xff
F_BIAS         = 126
FLT_MAX_EXP    = F_MAX_EXP - F_BIAS
FLT_MIN_EXP    = 1 - F_BIAS
FLT_MANT_DIG   = 24

INEXACT_RESULT 		= 0x0001
UNDERFLOW 		= 0x0002
OVERFLOW 		= 0x0004
DIVIDE_BY_ZERO 		= 0x0008
INVALID_OPERATION 	= 0x0010

SINGLE_FLOAT = 1

NOOP         = 0
ADD          = 1
MULTIPLY     = 2
DIVIDE       = 3
NEGATE       = 4
COMPARE      = 5
EXTENDSFDF   = 6
TRUNCDFSF    = 7

UNKNOWN           = -1
ROUND_TO_NEAREST  = 0 ;| round result to nearest representable value
ROUND_TO_ZERO     = 1 ;| round result towards zero
ROUND_TO_PLUS     = 2 ;| round result towards plus infinity
ROUND_TO_MINUS    = 3 ;| round result towards minus infinity

;| Entry points:

	.globl SYM (__addsf3)
	.globl SYM (__subsf3)
	.globl SYM (__mulsf3)
	.globl SYM (__divsf3)
	.globl SYM (__negsf2)
	.globl SYM (__cmpsf2)

;| These are common routines to return and signal exceptions.	

	.text
	.even

Lfden:
;| Return and signal a denormalized number
	or.l	d7,d0
	move.w	IMM (INEXACT_RESULT+UNDERFLOW),d7
	moveq	IMM (SINGLE_FLOAT),d6
	jmp	_exception_handler

Lfinfty:
Lfoverflow:
;| Return a properly signed INFINITY and set the exception flags 
	move.l	IMM (INFINITY),d0
	or.l	d7,d0
	move.w	IMM (INEXACT_RESULT+OVERFLOW),d7
	moveq	IMM (SINGLE_FLOAT),d6
	jmp	_exception_handler

Lfunderflow:
;| Return 0 and set the exception flags 
	move.l	IMM (0),d0
	move.w	IMM (INEXACT_RESULT+UNDERFLOW),d7
	moveq	IMM (SINGLE_FLOAT),d6
	jmp	_exception_handler

Lfinop:
;| Return a quiet NaN and set the exception flags
	move.l	IMM (QUIET_NaN),d0
	move.w	IMM (INEXACT_RESULT+INVALID_OPERATION),d7
	moveq	IMM (SINGLE_FLOAT),d6
	jmp	_exception_handler

Lfdiv0:
;| Return a properly signed INFINITY and set the exception flags
	move.l	IMM (INFINITY),d0
	or.l	d7,d0
	move.w	IMM (INEXACT_RESULT+DIVIDE_BY_ZERO),d7
	moveq	IMM (SINGLE_FLOAT),d6
	jmp	_exception_handler

;|=============================================================================
;|=============================================================================
;|                         single precision routines
;|=============================================================================
;|=============================================================================

;| A single precision floating point number (float) has the format:
;|
;| struct _float {
;|  unsigned int sign      : 1;  /* sign bit */ 
;|  unsigned int exponent  : 8;  /* exponent, shifted by 126 */
;|  unsigned int fraction  : 23; /* fraction */
;| } float;
;| 
;| Thus sizeof(float) = 4 (32 bits). 
;|
;| All the routines are callable from C programs, and return the result 
;| in the single register d0. They also preserve all registers except 
;| d0-d1 and a0-a1.

;|=============================================================================
;|                              __subsf3
;|=============================================================================

;| float __subsf3(float, float);
SYM (__subsf3):
	bchg	IMM (31),8(sp)	;| change sign of second operand
				;| and fall through
;|=============================================================================
;|                              __addsf3
;|=============================================================================

;| float __addsf3(float, float);
SYM (__addsf3):
#ifndef __mcf5200__
	link	a6,IMM (0)	;| everything will be done in registers
	movem.l	d2-d7,-(sp)	;| save all data registers but d0-d1
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.l	8(a6),d0	;| get first operand
	move.l	12(a6),d1	;| get second operand
	move.l	d0,d6		;| get d0's sign bit '
	add.l	d0,d0		;| check and clear sign bit of a
	beq	Laddsfb	;| if zero return second operand
	move.l	d1,d7		;| save b's sign bit '
	add.l	d1,d1		;| get rid of sign bit
	beq	Laddsfa	;| if zero return first operand

	move.l	d6,a0		;| save signs in address registers
	move.l	d7,a1		;| so we can use d6 and d7

;| Get the exponents and check for denormalized and/or infinity.

	move.l	IMM (0x00ffffff),d4	;| mask to get fraction
	move.l	IMM (0x01000000),d5	;| mask to put hidden bit back

	move.l	d0,d6		;| save a to get exponent
	and.l	d4,d0		;| get fraction in d0
	not.l 	d4		;| make d4 into a mask for the exponent
	and.l	d4,d6		;| get exponent in d6
	beq	Laddsfaden	;| branch if a is denormalized
	cmp.l	d4,d6		;| check for INFINITY or NaN
	beq	Laddsfnf
	swap	d6		;| put exponent into first word
	or.l	d5,d0		;| and put hidden bit back
Laddsf1:
;| Now we have a's exponent in d6 (second byte) and the mantissa in d0. '
	move.l	d1,d7		;| get exponent in d7
	and.l	d4,d7		;| 
	beq	Laddsfbden	;| branch if b is denormalized
	cmp.l	d4,d7		;| check for INFINITY or NaN
	beq	Laddsfnf
	swap	d7		;| put exponent into first word
	not.l 	d4		;| make d4 into a mask for the fraction
	and.l	d4,d1		;| get fraction in d1
	or.l	d5,d1		;| and put hidden bit back
Laddsf2:
;| Now we have b's exponent in d7 (second byte) and the mantissa in d1. '

;| Note that the hidden bit corresponds to bit #FLT_MANT_DIG-1, and we 
;| shifted right once, so bit #FLT_MANT_DIG is set (so we have one extra
;| bit).

	move.l	d1,d2		;| move b to d2, since we want to use
				;| two registers to do the sum
	move.l	IMM (0),d1	;| and clear the new ones
	move.l	d1,d3		;|

;| Here we shift the numbers in registers d0 and d1 so the exponents are the
;| same, and put the largest exponent in d6. Note that we are using two
;| registers for each number (see the discussion by D. Knuth in "Seminumerical 
;| Algorithms").
#ifndef __mcf5200__
	cmp.w	d6,d7		;| compare exponents
#else
	cmp.l	d6,d7		;| compare exponents
#endif
	beq	Laddsf3	;| if equal don't shift '
	bhi	5f		;| branch if second exponent largest
1:
	sub.l	d6,d7		;| keep the largest exponent
	neg.l	d7
#ifndef __mcf5200__
	lsr.w	IMM (8),d7	;| put difference in lower byte
#else
	lsr.l	IMM (8),d7	;| put difference in lower byte
#endif
;| if difference is too large we don't shift (actually, we can just exit) '
#ifndef __mcf5200__
	cmp.w	IMM (FLT_MANT_DIG+2),d7		
#else
	cmp.l	IMM (FLT_MANT_DIG+2),d7		
#endif
	bge	Laddsfbsmall
#ifndef __mcf5200__
	cmp.w	IMM (16),d7	;| if difference >= 16 swap
#else
	cmp.l	IMM (16),d7	;| if difference >= 16 swap
#endif
	bge	4f
2:
#ifndef __mcf5200__
	sub.w	IMM (1),d7
#else
	subq.l	IMM (1),d7
#endif
3:
#ifndef __mcf5200__
	lsr.l	IMM (1),d2	;| shift right second operand
	roxr.l	IMM (1),d3
	dbra	d7,3b
#else
	lsr.l	IMM (1),d3
	btst	IMM (0),d2
	beq	10f
	bset	IMM (31),d3
10:	lsr.l	IMM (1),d2
	subq.l	IMM (1),d7
	bpl	3b
#endif
	bra	Laddsf3
4:
	move.w	d2,d3
	swap	d3
	move.w	d3,d2
	swap	d2
#ifndef __mcf5200__
	sub.w	IMM (16),d7
#else
	sub.l	IMM (16),d7
#endif
	bne	2b		;| if still more bits, go back to normal case
	bra	Laddsf3
5:
#ifndef __mcf5200__
	exg	d6,d7		;| exchange the exponents
#else
	eor.l	d6,d7
	eor.l	d7,d6
	eor.l	d6,d7
#endif
	sub.l	d6,d7		;| keep the largest exponent
	neg.l	d7		;|
#ifndef __mcf5200__
	lsr.w	IMM (8),d7	;| put difference in lower byte
#else
	lsr.l	IMM (8),d7	;| put difference in lower byte
#endif
;| if difference is too large we don't shift (and exit!) '
#ifndef __mcf5200__
	cmp.w	IMM (FLT_MANT_DIG+2),d7		
#else
	cmp.l	IMM (FLT_MANT_DIG+2),d7		
#endif
	bge	Laddsfasmall
#ifndef __mcf5200__
	cmp.w	IMM (16),d7	;| if difference >= 16 swap
#else
	cmp.l	IMM (16),d7	;| if difference >= 16 swap
#endif
	bge	8f
6:
#ifndef __mcf5200__
	sub.w	IMM (1),d7
#else
	sub.l	IMM (1),d7
#endif
7:
#ifndef __mcf5200__
	lsr.l	IMM (1),d0	;| shift right first operand
	roxr.l	IMM (1),d1
	dbra	d7,7b
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
	subq.l	IMM (1),d7
	bpl	7b
#endif
	bra	Laddsf3
8:
	move.w	d0,d1
	swap	d1
	move.w	d1,d0
	swap	d0
#ifndef __mcf5200__
	sub.w	IMM (16),d7
#else
	sub.l	IMM (16),d7
#endif
	bne	6b		;| if still more bits, go back to normal case
				;| otherwise we fall through

;| Now we have a in d0-d1, b in d2-d3, and the largest exponent in d6 (the
;| signs are stored in a0 and a1).

Laddsf3:
;| Here we have to decide whether to add or subtract the numbers
#ifndef __mcf5200__
	exg	d6,a0		;| get signs back
	exg	d7,a1		;| and save the exponents
#else
	move.l	d6,d4
	move.l	a0,d6
	move.l	d4,a0
	move.l	d7,d4
	move.l	a1,d7
	move.l	d4,a1
#endif
	eor.l	d6,d7		;| combine sign bits
	bmi	Lsubsf0	;| if negative a and b have opposite 
				;| sign so we actually subtract the
				;| numbers

;| Here we have both positive or both negative
#ifndef __mcf5200__
	exg	d6,a0		;| now we have the exponent in d6
#else
	move.l	d6,d4
	move.l	a0,d6
	move.l	d4,a0
#endif
	move.l	a0,d7		;| and sign in d7
	and.l	IMM (0x80000000),d7
;| Here we do the addition.
	add.l	d3,d1
	addx.l	d2,d0
;| Note: now we have d2, d3, d4 and d5 to play with! 

;| Put the exponent, in the first byte, in d2, to use the "standard" rounding
;| routines:
	move.l	d6,d2
#ifndef __mcf5200__
	lsr.w	IMM (8),d2
#else
	lsr.l	IMM (8),d2
#endif

;| Before rounding normalize so bit #FLT_MANT_DIG is set (we will consider
;| the case of denormalized numbers in the rounding routine itself).
;| As in the addition (not in the subtraction!) we could have set 
;| one more bit we check this:
	btst	IMM (FLT_MANT_DIG+1),d0	
	beq	1f
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
#endif
	add.l	IMM (1),d2
1:
	lea	Laddsf4,a0	;| to return from rounding routine
	lea	SYM (_fpCCR),a1	;| check the rounding mode
#ifdef __mcf5200__
	clr.l	d6
#endif
	move.w	6(a1),d6	;| rounding mode in d6
	beq	Lroundtonearest
#ifndef __mcf5200__
	cmp.w	IMM (ROUND_TO_PLUS),d6
#else
	cmp.l	IMM (ROUND_TO_PLUS),d6
#endif
	bhi	Lroundtominus
	blt	Lroundtozero
	bra	Lroundtoplus
Laddsf4:
;| Put back the exponent, but check for overflow.
#ifndef __mcf5200__
	cmp.w	IMM (0xff),d2
#else
	cmp.l	IMM (0xff),d2
#endif
	bhi	1f
	bclr	IMM (FLT_MANT_DIG-1),d0
#ifndef __mcf5200__
	lsl.w	IMM (7),d2
#else
	lsl.l	IMM (7),d2
#endif
	swap	d2
	or.l	d2,d0
	bra	Laddsfret
1:
	move.w	IMM (ADD),d5
	bra	Lfoverflow

Lsubsf0:
;| We are here if a > 0 and b < 0 (sign bits cleared).
;| Here we do the subtraction.
	move.l	d6,d7		;| put sign in d7
	and.l	IMM (0x80000000),d7

	sub.l	d3,d1		;| result in d0-d1
	subx.l	d2,d0		;|
	beq	Laddsfret	;| if zero just exit
	bpl	1f		;| if positive skip the following
	bchg	IMM (31),d7	;| change sign bit in d7
	neg.l	d1
	negx.l	d0
1:
#ifndef __mcf5200__
	exg	d2,a0		;| now we have the exponent in d2
	lsr.w	IMM (8),d2	;| put it in the first byte
#else
	move.l	d2,d4
	move.l	a0,d2
	move.l	d4,a0
	lsr.l	IMM (8),d2	;| put it in the first byte
#endif

;| Now d0-d1 is positive and the sign bit is in d7.

;| Note that we do not have to normalize, since in the subtraction bit
;| #FLT_MANT_DIG+1 is never set, and denormalized numbers are handled by
;| the rounding routines themselves.
	lea	Lsubsf1,a0	;| to return from rounding routine
	lea	SYM (_fpCCR),a1	;| check the rounding mode
#ifdef __mcf5200__
	clr.l	d6
#endif
	move.w	6(a1),d6	;| rounding mode in d6
	beq	Lroundtonearest
#ifndef __mcf5200__
	cmp.w	IMM (ROUND_TO_PLUS),d6
#else
	cmp.l	IMM (ROUND_TO_PLUS),d6
#endif
	bhi	Lroundtominus
	blt	Lroundtozero
	bra	Lroundtoplus
Lsubsf1:
;| Put back the exponent (we can't have overflow!). '
	bclr	IMM (FLT_MANT_DIG-1),d0
#ifndef __mcf5200__
	lsl.w	IMM (7),d2
#else
	lsl.l	IMM (7),d2
#endif
	swap	d2
	or.l	d2,d0
	bra	Laddsfret

;| If one of the numbers was too small (difference of exponents >= 
;| FLT_MANT_DIG+2) we return the other (and now we don't have to '
;| check for finiteness or zero).
Laddsfasmall:
	move.l	12(a6),d0
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| restore data registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| and return
	rts

Laddsfbsmall:
	move.l	8(a6),d0
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| restore data registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| and return
	rts

;| If the numbers are denormalized remember to put exponent equal to 1.

Laddsfaden:
	move.l	d5,d6		;| d5 contains 0x01000000
	swap	d6
	bra	Laddsf1

Laddsfbden:
	move.l	d5,d7
	swap	d7
	not.l 	d4		;| make d4 into a mask for the fraction
				;| (this was not executed after the jump)
	bra	Laddsf2

;| The rest is mainly code for the different results which can be 
;| returned (checking always for +/-INFINITY and NaN).

Laddsfb:
;| Return b (if a is zero).
	move.l	12(a6),d0
	bra	1f
Laddsfa:
;| Return a (if b is zero).
	move.l	8(a6),d0
1:
	move.w	IMM (ADD),d5
;| We have to check for NaN and +/-infty.
	move.l	d0,d7
	and.l	IMM (0x80000000),d7	;| put sign in d7
	bclr	IMM (31),d0		;| clear sign
	cmp.l	IMM (INFINITY),d0	;| check for infty or NaN
	bge	2f
	move.l	d0,d0		;| check for zero (we do this because we don't '
	bne	Laddsfret	;| want to return -0 by mistake
	bclr	IMM (31),d7	;| if zero be sure to clear sign
	bra	Laddsfret	;| if everything OK just return
2:
;| The value to be returned is either +/-infty or NaN
	and.l	IMM (0x007fffff),d0	;| check for NaN
	bne	Lfinop			;| if mantissa not zero is NaN
	bra	Lfinfty

Laddsfret:
;| Normal exit (a and b nonzero, result is not NaN nor +/-infty).
;| We have to clear the exception flags (just the exception type).
	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
	or.l	d7,d0		;| put sign bit
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| restore data registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| and return
	rts

Laddsfretden:
;| Return a denormalized number (for addition we don't signal underflow) '
	lsr.l	IMM (1),d0	;| remember to shift right back once
	bra	Laddsfret	;| and return

;| Note: when adding two floats of the same sign if either one is 
;| NaN we return NaN without regard to whether the other is finite or 
;| not. When subtracting them (i.e., when adding two numbers of 
;| opposite signs) things are more complicated: if both are INFINITY 
;| we return NaN, if only one is INFINITY and the other is NaN we return
;| NaN, but if it is finite we return INFINITY with the corresponding sign.

Laddsfnf:
	move.w	IMM (ADD),d5
;| This could be faster but it is not worth the effort, since it is not
;| executed very often. We sacrifice speed for clarity here.
	move.l	8(a6),d0	;| get the numbers back (remember that we
	move.l	12(a6),d1	;| did some processing already)
	move.l	IMM (INFINITY),d4 ;| useful constant (INFINITY)
	move.l	d0,d2		;| save sign bits
	move.l	d1,d3
	bclr	IMM (31),d0	;| clear sign bits
	bclr	IMM (31),d1
;| We know that one of them is either NaN of +/-INFINITY
;| Check for NaN (if either one is NaN return NaN)
	cmp.l	d4,d0		;| check first a (d0)
	bhi	Lfinop		
	cmp.l	d4,d1		;| check now b (d1)
	bhi	Lfinop		
;| Now comes the check for +/-INFINITY. We know that both are (maybe not
;| finite) numbers, but we have to check if both are infinite whether we
;| are adding or subtracting them.
	eor.l	d3,d2		;| to check sign bits
	bmi	1f
	move.l	d0,d7
	and.l	IMM (0x80000000),d7	;| get (common) sign bit
	bra	Lfinfty
1:
;| We know one (or both) are infinite, so we test for equality between the
;| two numbers (if they are equal they have to be infinite both, so we
;| return NaN).
	cmp.l	d1,d0		;| are both infinite?
	beq	Lfinop		;| if so return NaN

	move.l	d0,d7
	and.l	IMM (0x80000000),d7 ;| get a's sign bit '
	cmp.l	d4,d0		;| test now for infinity
	beq	Lfinfty	;| if a is INFINITY return with this sign
	bchg	IMM (31),d7	;| else we know b is INFINITY and has
	bra	Lfinfty	;| the opposite sign

;|=============================================================================
;|                             __mulsf3
;|=============================================================================

;| float __mulsf3(float, float);
SYM (__mulsf3):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.l	8(a6),d0	;| get a into d0
	move.l	12(a6),d1	;| and b into d1
	move.l	d0,d7		;| d7 will hold the sign of the product
	eor.l	d1,d7		;|
	and.l	IMM (0x80000000),d7
	move.l	IMM (INFINITY),d6	;| useful constant (+INFINITY)
	move.l	d6,d5			;| another (mask for fraction)
	not.l	d5			;|
	move.l	IMM (0x00800000),d4	;| this is to put hidden bit back
	bclr	IMM (31),d0		;| get rid of a's sign bit '
	move.l	d0,d2			;|
	beq	Lmulsfa0		;| branch if a is zero
	bclr	IMM (31),d1		;| get rid of b's sign bit '
	move.l	d1,d3		;|
	beq	Lmulsfb0	;| branch if b is zero
	cmp.l	d6,d0		;| is a big?
	bhi	Lmulsfinop	;| if a is NaN return NaN
	beq	Lmulsfinf	;| if a is INFINITY we have to check b
	cmp.l	d6,d1		;| now compare b with INFINITY
	bhi	Lmulsfinop	;| is b NaN?
	beq	Lmulsfoverflow ;| is b INFINITY?
;| Here we have both numbers finite and nonzero (and with no sign bit).
;| Now we get the exponents into d2 and d3.
	and.l	d6,d2		;| and isolate exponent in d2
	beq	Lmulsfaden	;| if exponent is zero we have a denormalized
	and.l	d5,d0		;| and isolate fraction
	or.l	d4,d0		;| and put hidden bit back
	swap	d2		;| I like exponents in the first byte
#ifndef __mcf5200__
	lsr.w	IMM (7),d2	;| 
#else
	lsr.l	IMM (7),d2	;| 
#endif
Lmulsf1:			;| number
	and.l	d6,d3		;|
	beq	Lmulsfbden	;|
	and.l	d5,d1		;|
	or.l	d4,d1		;|
	swap	d3		;|
#ifndef __mcf5200__
	lsr.w	IMM (7),d3	;|
#else
	lsr.l	IMM (7),d3	;|
#endif
Lmulsf2:			;|
#ifndef __mcf5200__
	add.w	d3,d2		;| add exponents
	sub.w	IMM (F_BIAS+1),d2 ;| and subtract bias (plus one)
#else
	add.l	d3,d2		;| add exponents
	sub.l	IMM (F_BIAS+1),d2 ;| and subtract bias (plus one)
#endif

;| We are now ready to do the multiplication. The situation is as follows:
;| both a and b have bit FLT_MANT_DIG-1 set (even if they were 
;| denormalized to start with!), which means that in the product 
;| bit 2*(FLT_MANT_DIG-1) (that is, bit 2*FLT_MANT_DIG-2-32 of the 
;| high long) is set. 

;| To do the multiplication let us move the number a little bit around ...
	move.l	d1,d6		;| second operand in d6
	move.l	d0,d5		;| first operand in d4-d5
	move.l	IMM (0),d4
	move.l	d4,d1		;| the sums will go in d0-d1
	move.l	d4,d0

;| now bit FLT_MANT_DIG-1 becomes bit 31:
	lsl.l	IMM (31-FLT_MANT_DIG+1),d6		

;| Start the loop (we loop #FLT_MANT_DIG times):
	move.w	IMM (FLT_MANT_DIG-1),d3	
1:	add.l	d1,d1		;| shift sum 
	addx.l	d0,d0
	lsl.l	IMM (1),d6	;| get bit bn
	bcc	2f		;| if not set skip sum
	add.l	d5,d1		;| add a
	addx.l	d4,d0
2:
#ifndef __mcf5200__
	dbf	d3,1b		;| loop back
#else
	subq.l	IMM (1),d3
	bpl	1b
#endif

;| Now we have the product in d0-d1, with bit (FLT_MANT_DIG - 1) + FLT_MANT_DIG
;| (mod 32) of d0 set. The first thing to do now is to normalize it so bit 
;| FLT_MANT_DIG is set (to do the rounding).
#ifndef __mcf5200__
	ror.l	IMM (6),d1
	swap	d1
	move.w	d1,d3
	and.w	IMM (0x03ff),d3
	and.w	IMM (0xfd00),d1
#else
	move.l	d1,d3
	lsl.l	IMM (8),d1
	add.l	d1,d1
	add.l	d1,d1
	moveq	IMM (22),d5
	lsr.l	d5,d3
	or.l	d3,d1
	and.l	IMM (0xfffffd00),d1
#endif
	lsl.l	IMM (8),d0
	add.l	d0,d0
	add.l	d0,d0
#ifndef __mcf5200__
	or.w	d3,d0
#else
	or.l	d3,d0
#endif

	move.w	IMM (MULTIPLY),d5
	
	btst	IMM (FLT_MANT_DIG+1),d0
	beq	Lroundexit
#ifndef __mcf5200__
	lsr.l	IMM (1),d0
	roxr.l	IMM (1),d1
	add.w	IMM (1),d2
#else
	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
	addq.l	IMM (1),d2
#endif
	bra	Lroundexit

Lmulsfinop:
	move.w	IMM (MULTIPLY),d5
	bra	Lfinop

Lmulsfoverflow:
	move.w	IMM (MULTIPLY),d5
	bra	Lfoverflow

Lmulsfinf:
	move.w	IMM (MULTIPLY),d5
;| If either is NaN return NaN; else both are (maybe infinite) numbers, so
;| return INFINITY with the correct sign (which is in d7).
	cmp.l	d6,d1		;| is b NaN?
	bhi	Lfinop		;| if so return NaN
	bra	Lfoverflow	;| else return +/-INFINITY

;| If either number is zero return zero, unless the other is +/-INFINITY, 
;| or NaN, in which case we return NaN.
Lmulsfb0:
;| Here d1 (==b) is zero.
	move.l	d1,d0		;| put b into d0 (just a zero)
	move.l	8(a6),d1	;| get a again to check for non-finiteness
	bra	1f
Lmulsfa0:
	move.l	12(a6),d1	;| get b again to check for non-finiteness
1:	bclr	IMM (31),d1	;| clear sign bit 
	cmp.l	IMM (INFINITY),d1 ;| and check for a large exponent
	bge	Lfinop		;| if b is +/-INFINITY or NaN return NaN
	lea	SYM (_fpCCR),a0	;| else return zero
	move.w	IMM (0),(a0)	;| 
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7	;| 
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6		;| 
	rts			;| 

;| If a number is denormalized we put an exponent of 1 but do not put the 
;| hidden bit back into the fraction; instead we shift left until bit 23
;| (the hidden bit) is set, adjusting the exponent accordingly. We do this
;| to ensure that the product of the fractions is close to 1.
Lmulsfaden:
	move.l	IMM (1),d2
	and.l	d5,d0
1:	add.l	d0,d0		;| shift a left (until bit 23 is set)
#ifndef __mcf5200__
	sub.w	IMM (1),d2	;| and adjust exponent
#else
	subq.l	IMM (1),d2	;| and adjust exponent
#endif
	btst	IMM (FLT_MANT_DIG-1),d0
	bne	Lmulsf1	;|
	bra	1b		;| else loop back

Lmulsfbden:
	move.l	IMM (1),d3
	and.l	d5,d1
1:	add.l	d1,d1		;| shift b left until bit 23 is set
#ifndef __mcf5200__
	sub.w	IMM (1),d3	;| and adjust exponent
#else
	sub.l	IMM (1),d3	;| and adjust exponent
#endif
	btst	IMM (FLT_MANT_DIG-1),d1
	bne	Lmulsf2	;|
	bra	1b		;| else loop back

;|=============================================================================
;|                             __divsf3
;|=============================================================================

;| float __divsf3(float, float);
SYM (__divsf3):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.l	8(a6),d0		;| get a into d0
	move.l	12(a6),d1		;| and b into d1
	move.l	d0,d7			;| d7 will hold the sign of the result
	eor.l	d1,d7			;|
	and.l	IMM (0x80000000),d7	;| 
	move.l	IMM (INFINITY),d6	;| useful constant (+INFINITY)
	move.l	d6,d5			;| another (mask for fraction)
	not.l	d5			;|
	move.l	IMM (0x00800000),d4	;| this is to put hidden bit back
	bclr	IMM (31),d0		;| get rid of a's sign bit '
	move.l	d0,d2			;|
	beq	Ldivsfa0		;| branch if a is zero
	bclr	IMM (31),d1		;| get rid of b's sign bit '
	move.l	d1,d3			;|
	beq	Ldivsfb0		;| branch if b is zero
	cmp.l	d6,d0			;| is a big?
	bhi	Ldivsfinop		;| if a is NaN return NaN
	beq	Ldivsfinf		;| if a is INFINITY we have to check b
	cmp.l	d6,d1			;| now compare b with INFINITY 
	bhi	Ldivsfinop		;| if b is NaN return NaN
	beq	Ldivsfunderflow
;| Here we have both numbers finite and nonzero (and with no sign bit).
;| Now we get the exponents into d2 and d3 and normalize the numbers to
;| ensure that the ratio of the fractions is close to 1. We do this by
;| making sure that bit #FLT_MANT_DIG-1 (hidden bit) is set.
	and.l	d6,d2		;| and isolate exponent in d2
	beq	Ldivsfaden	;| if exponent is zero we have a denormalized
	and.l	d5,d0		;| and isolate fraction
	or.l	d4,d0		;| and put hidden bit back
	swap	d2		;| I like exponents in the first byte
#ifndef __mcf5200__
	lsr.w	IMM (7),d2	;| 
#else
	lsr.l	IMM (7),d2	;| 
#endif
Ldivsf1:			;| 
	and.l	d6,d3		;|
	beq	Ldivsfbden	;|
	and.l	d5,d1		;|
	or.l	d4,d1		;|
	swap	d3		;|
#ifndef __mcf5200__
	lsr.w	IMM (7),d3	;|
#else
	lsr.l	IMM (7),d3	;|
#endif
Ldivsf2:			;|
#ifndef __mcf5200__
	sub.w	d3,d2		;| subtract exponents
 	add.w	IMM (F_BIAS),d2	;| and add bias
#else
	sub.l	d3,d2		;| subtract exponents
 	add.l	IMM (F_BIAS),d2	;| and add bias
#endif
 
;| We are now ready to do the division. We have prepared things in such a way
;| that the ratio of the fractions will be less than 2 but greater than 1/2.
;| At this point the registers in use are:
;| d0	holds a (first operand, bit FLT_MANT_DIG=0, bit FLT_MANT_DIG-1=1)
;| d1	holds b (second operand, bit FLT_MANT_DIG=1)
;| d2	holds the difference of the exponents, corrected by the bias
;| d7	holds the sign of the ratio
;| d4, d5, d6 hold some constants
	move.l	d7,a0		;| d6-d7 will hold the ratio of the fractions
	move.l	IMM (0),d6	;| 
	move.l	d6,d7

	move.w	IMM (FLT_MANT_DIG+1),d3
1:	cmp.l	d0,d1		;| is a < b?
	bhi	2f		;|
	bset	d3,d6		;| set a bit in d6
	sub.l	d1,d0		;| if a >= b  a <-- a-b
	beq	3f		;| if a is zero, exit
2:	add.l	d0,d0		;| multiply a by 2
#ifndef __mcf5200__
	dbra	d3,1b
#else
	subq.l	IMM (1),d3
	bpl	1b
#endif

;| Now we keep going to set the sticky bit ...
	move.w	IMM (FLT_MANT_DIG),d3
1:	cmp.l	d0,d1
	ble	2f
	add.l	d0,d0
#ifndef __mcf5200__
	dbra	d3,1b
#else
	subq.l	IMM(1),d3
	bpl	1b
#endif
	move.l	IMM (0),d1
	bra	3f
2:	move.l	IMM (0),d1
#ifndef __mcf5200__
	sub.w	IMM (FLT_MANT_DIG),d3
	add.w	IMM (31),d3
#else
	sub.l	IMM (FLT_MANT_DIG),d3
	add.l	IMM (31),d3
#endif
	bset	d3,d1
3:
	move.l	d6,d0		;| put the ratio in d0-d1
	move.l	a0,d7		;| get sign back

;| Because of the normalization we did before we are guaranteed that 
;| d0 is smaller than 2^26 but larger than 2^24. Thus bit 26 is not set,
;| bit 25 could be set, and if it is not set then bit 24 is necessarily set.
	btst	IMM (FLT_MANT_DIG+1),d0		
	beq	1f              ;| if it is not set, then bit 24 is set
	lsr.l	IMM (1),d0	;|
#ifndef __mcf5200__
	add.w	IMM (1),d2	;|
#else
	add.l	IMM (1),d2	;|
#endif
1:
;| Now round, check for over- and underflow, and exit.
	move.w	IMM (DIVIDE),d5
	bra	Lroundexit

Ldivsfinop:
	move.w	IMM (DIVIDE),d5
	bra	Lfinop

Ldivsfoverflow:
	move.w	IMM (DIVIDE),d5
	bra	Lfoverflow

Ldivsfunderflow:
	move.w	IMM (DIVIDE),d5
	bra	Lfunderflow

Ldivsfa0:
	move.w	IMM (DIVIDE),d5
;| If a is zero check to see whether b is zero also. In that case return
;| NaN; then check if b is NaN, and return NaN also in that case. Else
;| return zero.
	and.l	IMM (0x7fffffff),d1	;| clear sign bit and test b
	beq	Lfinop			;| if b is also zero return NaN
	cmp.l	IMM (INFINITY),d1	;| check for NaN
	bhi	Lfinop			;| 
	move.l	IMM (0),d0		;| else return zero
	lea	SYM (_fpCCR),a0		;|
	move.w	IMM (0),(a0)		;|
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7		;| 
#else
	movem.l	(sp),d2-d7		;| 
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6			;| 
	rts				;| 
	
Ldivsfb0:
	move.w	IMM (DIVIDE),d5
;| If we got here a is not zero. Check if a is NaN; in that case return NaN,
;| else return +/-INFINITY. Remember that a is in d0 with the sign bit 
;| cleared already.
	cmp.l	IMM (INFINITY),d0	;| compare d0 with INFINITY
	bhi	Lfinop			;| if larger it is NaN
	bra	Lfdiv0		;| else signal DIVIDE_BY_ZERO

Ldivsfinf:
	move.w	IMM (DIVIDE),d5
;| If a is INFINITY we have to check b
	cmp.l	IMM (INFINITY),d1	;| compare b with INFINITY 
	bge	Lfinop			;| if b is NaN or INFINITY return NaN
	bra	Lfoverflow		;| else return overflow

;| If a number is denormalized we put an exponent of 1 but do not put the 
;| bit back into the fraction.
Ldivsfaden:
	move.l	IMM (1),d2
	and.l	d5,d0
1:	add.l	d0,d0		;| shift a left until bit FLT_MANT_DIG-1 is set
#ifndef __mcf5200__
	sub.w	IMM (1),d2	;| and adjust exponent
#else
	sub.l	IMM (1),d2	;| and adjust exponent
#endif
	btst	IMM (FLT_MANT_DIG-1),d0
	bne	Ldivsf1
	bra	1b

Ldivsfbden:
	move.l	IMM (1),d3
	and.l	d5,d1
1:	add.l	d1,d1		;| shift b left until bit FLT_MANT_DIG is set
#ifndef __mcf5200__
	sub.w	IMM (1),d3	;| and adjust exponent
#else
	sub.l	IMM (1),d3	;| and adjust exponent
#endif
	btst	IMM (FLT_MANT_DIG-1),d1
	bne	Ldivsf2
	bra	1b

Lroundexit:
;| This is a common exit point for __mulsf3 and __divsf3. 

;| First check for underlow in the exponent:
#ifndef __mcf5200__
	cmp.w	IMM (-FLT_MANT_DIG-1),d2		
#else
	cmp.l	IMM (-FLT_MANT_DIG-1),d2		
#endif
	blt	Lfunderflow	
;| It could happen that the exponent is less than 1, in which case the 
;| number is denormalized. In this case we shift right and adjust the 
;| exponent until it becomes 1 or the fraction is zero (in the latter case 
;| we signal underflow and return zero).
	move.l	IMM (0),d6	;| d6 is used temporarily
#ifndef __mcf5200__
	cmp.w	IMM (1),d2	;| if the exponent is less than 1 we 
#else
	cmp.l	IMM (1),d2	;| if the exponent is less than 1 we 
#endif
	bge	2f		;| have to shift right (denormalize)
1:
#ifndef __mcf5200__
	add.w	IMM (1),d2	;| adjust the exponent
	lsr.l	IMM (1),d0	;| shift right once 
	roxr.l	IMM (1),d1	;|
	roxr.l	IMM (1),d6	;| d6 collect bits we would lose otherwise
	cmp.w	IMM (1),d2	;| is the exponent 1 already?
#else
	addq.l	IMM (1),d2	;| adjust the exponent
	lsr.l	IMM (1),d6
	btst	IMM (0),d1
	beq	11f
	bset	IMM (31),d6
11:	lsr.l	IMM (1),d1
	btst	IMM (0),d0
	beq	10f
	bset	IMM (31),d1
10:	lsr.l	IMM (1),d0
	cmp.l	IMM (1),d2	;| is the exponent 1 already?
#endif
	beq	2f		;| if not loop back
	bra	1b              ;|
	bra	Lfunderflow	;| safety check, shouldn't execute '
2:	or.l	d6,d1		;| this is a trick so we don't lose  '
				;| the extra bits which were flushed right
;| Now call the rounding routine (which takes care of denormalized numbers):
	lea	Lround0,a0	;| to return from rounding routine
	lea	SYM (_fpCCR),a1	;| check the rounding mode
#ifdef __mcf5200__
	clr.l	d6
#endif
	move.w	6(a1),d6	;| rounding mode in d6
	beq	Lroundtonearest
#ifndef __mcf5200__
	cmp.w	IMM (ROUND_TO_PLUS),d6
#else
	cmp.l	IMM (ROUND_TO_PLUS),d6
#endif
	bhi	Lroundtominus
	blt	Lroundtozero
	bra	Lroundtoplus
Lround0:
;| Here we have a correctly rounded result (either normalized or denormalized).

;| Here we should have either a normalized number or a denormalized one, and
;| the exponent is necessarily larger or equal to 1 (so we don't have to  '
;| check again for underflow!). We have to check for overflow or for a 
;| denormalized number (which also signals underflow).
;| Check for overflow (i.e., exponent >= 255).
#ifndef __mcf5200__
	cmp.w	IMM (0x00ff),d2
#else
	cmp.l	IMM (0x00ff),d2
#endif
	bge	Lfoverflow
;| Now check for a denormalized number (exponent==0).
	move.w	d2,d2
	beq	Lfden
1:
;| Put back the exponents and sign and return.
#ifndef __mcf5200__
	lsl.w	IMM (7),d2	;| exponent back to fourth byte
#else
	lsl.l	IMM (7),d2	;| exponent back to fourth byte
#endif
	bclr	IMM (FLT_MANT_DIG-1),d0
	swap	d0		;| and put back exponent
#ifndef __mcf5200__
	or.w	d2,d0		;| 
#else
	or.l	d2,d0
#endif
	swap	d0		;|
	or.l	d7,d0		;| and sign also

	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts

;|=============================================================================
;|                             __negsf2
;|=============================================================================

;| This is trivial and could be shorter if we didn't bother checking for NaN '
;| and +/-INFINITY.

;| float __negsf2(float);
SYM (__negsf2):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp)
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.w	IMM (NEGATE),d5
	move.l	8(a6),d0	;| get number to negate in d0
	bchg	IMM (31),d0	;| negate
	move.l	d0,d1		;| make a positive copy
	bclr	IMM (31),d1	;|
	tst.l	d1		;| check for zero
	beq	2f		;| if zero (either sign) return +zero
	cmp.l	IMM (INFINITY),d1 ;| compare to +INFINITY
	blt	1f		;|
	bhi	Lfinop		;| if larger (fraction not zero) is NaN
	move.l	d0,d7		;| else get sign and return INFINITY
	and.l	IMM (0x80000000),d7
	bra	Lfinfty		
1:	lea	SYM (_fpCCR),a0
	move.w	IMM (0),(a0)
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts
2:	bclr	IMM (31),d0
	bra	1b

;|=============================================================================
;|                             __cmpsf2
;|=============================================================================

GREATER =  1
LESS    = -1
EQUAL   =  0

;| int __cmpsf2(float, float);
SYM (__cmpsf2):
#ifndef __mcf5200__
	link	a6,IMM (0)
	movem.l	d2-d7,-(sp) 	;| save registers
#else
	link	a6,IMM (-24)
	movem.l	d2-d7,(sp)
#endif
	move.w	IMM (COMPARE),d5
	move.l	8(a6),d0	;| get first operand
	move.l	12(a6),d1	;| get second operand
;| Check if either is NaN, and in that case return garbage and signal
;| INVALID_OPERATION. Check also if either is zero, and clear the signs
;| if necessary.
	move.l	d0,d6
	and.l	IMM (0x7fffffff),d0
	beq	Lcmpsfa0
	cmp.l	IMM (0x7f800000),d0
	bhi	Lfinop
Lcmpsf1:
	move.l	d1,d7
	and.l	IMM (0x7fffffff),d1
	beq	Lcmpsfb0
	cmp.l	IMM (0x7f800000),d1
	bhi	Lfinop
Lcmpsf2:
;| Check the signs
	eor.l	d6,d7
	bpl	1f
;| If the signs are not equal check if a >= 0
	tst.l	d6
	bpl	Lcmpsfagtb	;| if (a >= 0 && b < 0) => a > b
	bmi	Lcmpsfbgta	;| if (a < 0 && b >= 0) => a < b
1:
;| If the signs are equal check for < 0
	tst.l	d6
	bpl	1f
;| If both are negative exchange them
#ifndef __mcf5200__
	exg	d0,d1
#else
	move.l	d0,d7
	move.l	d1,d0
	move.l	d7,d1
#endif
1:
;| Now that they are positive we just compare them as longs (does this also
;| work for denormalized numbers?).
	cmp.l	d0,d1
	bhi	Lcmpsfbgta	;| ;|b;| > ;|a;|
	bne	Lcmpsfagtb	;| ;|b;| < ;|a;|
;| If we got here a == b.
	move.l	IMM (EQUAL),d0
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7 	;| put back the registers
#else
	movem.l	(sp),d2-d7
#endif
	unlk	a6
	rts
Lcmpsfagtb:
	move.l	IMM (GREATER),d0
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7 	;| put back the registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts
Lcmpsfbgta:
	move.l	IMM (LESS),d0
#ifndef __mcf5200__
	movem.l	(sp)+,d2-d7 	;| put back the registers
#else
	movem.l	(sp),d2-d7
	;| XXX if frame pointer is ever removed, stack pointer must
	;| be adjusted here.
#endif
	unlk	a6
	rts

Lcmpsfa0:	
	bclr	IMM (31),d6
	bra	Lcmpsf1
Lcmpsfb0:
	bclr	IMM (31),d7
	bra	Lcmpsf2

;|=============================================================================
;|                           rounding routines
;|=============================================================================

;| The rounding routines expect the number to be normalized in registers
;| d0-d1, with the exponent in register d2. They assume that the 
;| exponent is larger or equal to 1. They return a properly normalized number
;| if possible, and a denormalized number otherwise. The exponent is returned
;| in d2.

Lroundtonearest:
;| We now normalize as suggested by D. Knuth ("Seminumerical Algorithms"):
;| Here we assume that the exponent is not too small (this should be checked
;| before entering the rounding routine), but the number could be denormalized.

;| Check for denormalized numbers:
1:	btst	IMM (FLT_MANT_DIG),d0
	bne	2f		;| if set the number is normalized
;| Normalize shifting left until bit #FLT_MANT_DIG is set or the exponent 
;| is one (remember that a denormalized number corresponds to an 
;| exponent of -F_BIAS+1).
#ifndef __mcf5200__
	cmp.w	IMM (1),d2	;| remember that the exponent is at least one
#else
	cmp.l	IMM (1),d2	;| remember that the exponent is at least one
#endif
 	beq	2f		;| an exponent of one means denormalized
	add.l	d1,d1		;| else shift and adjust the exponent
	addx.l	d0,d0		;|
#ifndef __mcf5200__
	dbra	d2,1b		;|
#else
	subq.l	IMM (1),d2
	bpl	1b
#endif
2:
;| Now round: we do it as follows: after the shifting we can write the
;| fraction part as f + delta, where 1 < f < 2^25, and 0 <= delta <= 2.
;| If delta < 1, do nothing. If delta > 1, add 1 to f. 
;| If delta == 1, we make sure the rounded number will be even (odd?) 
;| (after shifting).
	btst	IMM (0),d0	;| is delta < 1?
	beq	2f		;| if so, do not do anything
	tst.l	d1		;| is delta == 1?
	bne	1f		;| if so round to even
	move.l	d0,d1		;| 
	and.l	IMM (2),d1	;| bit 1 is the last significant bit
	add.l	d1,d0		;| 
	bra	2f		;| 
1:	move.l	IMM (1),d1	;| else add 1 
	add.l	d1,d0		;|
;| Shift right once (because we used bit #FLT_MANT_DIG!).
2:	lsr.l	IMM (1),d0		
;| Now check again bit #FLT_MANT_DIG (rounding could have produced a
;| 'fraction overflow' ...).
	btst	IMM (FLT_MANT_DIG),d0	
	beq	1f
	lsr.l	IMM (1),d0
#ifndef __mcf5200__
	add.w	IMM (1),d2
#else
	addq.l	IMM (1),d2
#endif
1:
;| If bit #FLT_MANT_DIG-1 is clear we have a denormalized number, so we 
;| have to put the exponent to zero and return a denormalized number.
	btst	IMM (FLT_MANT_DIG-1),d0
	beq	1f
	jmp	(a0)
1:	move.l	IMM (0),d2
	jmp	(a0)

Lroundtozero:
Lroundtoplus:
Lroundtominus:
	jmp	(a0)
#endif /* L_float */

;| gcc expects the routines __eqdf2, __nedf2, __gtdf2, __gedf2,
;| __ledf2, __ltdf2 to all return the same value as a direct call to
;| __cmpdf2 would.  In this implementation, each of these routines
;| simply calls __cmpdf2.  It would be more efficient to give the
;| __cmpdf2 routine several names, but separating them out will make it
;| easier to write efficient versions of these routines someday.

#ifdef  L_eqdf2
	.text
	/*.proc*/
	.globl	SYM (__eqdf2)
SYM (__eqdf2):
	link	a6,IMM (0)
	move.l	20(a6),-(sp)
	move.l	16(a6),-(sp)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpdf2)
	unlk	a6
	rts
#endif /* L_eqdf2 */

#ifdef  L_nedf2
	.text
	/*.proc*/
	.globl	SYM (__nedf2)
SYM (__nedf2):
	link	a6,IMM (0)
	move.l	20(a6),-(sp)
	move.l	16(a6),-(sp)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpdf2)
	unlk	a6
	rts
#endif /* L_nedf2 */

#ifdef  L_gtdf2
	.text
	/*.proc*/
	.globl	SYM (__gtdf2)
SYM (__gtdf2):
	link	a6,IMM (0)
	move.l	20(a6),-(sp)
	move.l	16(a6),-(sp)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpdf2)
	unlk	a6
	rts
#endif /* L_gtdf2 */

#ifdef  L_gedf2
	.text
	/*.proc*/
	.globl	SYM (__gedf2)
SYM (__gedf2):
	link	a6,IMM (0)
	move.l	20(a6),-(sp)
	move.l	16(a6),-(sp)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpdf2)
	unlk	a6
	rts
#endif /* L_gedf2 */

#ifdef  L_ltdf2
	.text
	/*.proc*/
	.globl	SYM (__ltdf2)
SYM (__ltdf2):
	link	a6,IMM (0)
	move.l	20(a6),-(sp)
	move.l	16(a6),-(sp)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpdf2)
	unlk	a6
	rts
#endif /* L_ltdf2 */

#ifdef  L_ledf2
	.text
	/*.proc*/
	.globl	SYM (__ledf2)
SYM (__ledf2):
	link	a6,IMM (0)
	move.l	20(a6),-(sp)
	move.l	16(a6),-(sp)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpdf2)
	unlk	a6
	rts
#endif /* L_ledf2 */

;| The comments above about __eqdf2, et. al., also apply to __eqsf2,
;| et. al., except that the latter call __cmpsf2 rather than __cmpdf2.

#ifdef  L_eqsf2
	.text
	/*.proc*/
	.globl	SYM (__eqsf2)
SYM (__eqsf2):
	link	a6,IMM (0)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpsf2)
	unlk	a6
	rts
#endif /* L_eqsf2 */

#ifdef  L_nesf2
	.text
	/*.proc*/
	.globl	SYM (__nesf2)
SYM (__nesf2):
	link	a6,IMM (0)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpsf2)
	unlk	a6
	rts
#endif /* L_nesf2 */

#ifdef  L_gtsf2
	.text
	/*.proc*/
	.globl	SYM (__gtsf2)
SYM (__gtsf2):
	link	a6,IMM (0)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpsf2)
	unlk	a6
	rts
#endif /* L_gtsf2 */

#ifdef  L_gesf2
	.text
	/*.proc*/
	.globl	SYM (__gesf2)
SYM (__gesf2):
	link	a6,IMM (0)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpsf2)
	unlk	a6
	rts
#endif /* L_gesf2 */

#ifdef  L_ltsf2
	.text
	/*.proc*/
	.globl	SYM (__ltsf2)
SYM (__ltsf2):
	link	a6,IMM (0)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpsf2)
	unlk	a6
	rts
#endif /* L_ltsf2 */

#ifdef  L_lesf2
	.text
	/*.proc*/
	.globl	SYM (__lesf2)
SYM (__lesf2):
	link	a6,IMM (0)
	move.l	12(a6),-(sp)
	move.l	8(a6),-(sp)
	bsr	SYM (__cmpsf2)
	unlk	a6
	rts
#endif /* L_lesf2 */
