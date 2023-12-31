/* Definitions for ARM running Linux-based GNU systems
   using ELF and 26-bit APCS.
   Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Philip Blundell <Philip.Blundell@pobox.com>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Tell linux-elf.h to default to 26-bit mode.  */
#define SUBTARGET_DEFAULT_APCS26
