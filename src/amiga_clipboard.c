/* amiga_clipboard.c --  code for interfacing with the Amigas iffparse.library
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <clib/iffparse_protos.h>
#include <libraries/iffparse.h>

_PR void  clip_kill(void);
_PR bool  write_clip(long, u_char *, long);
_PR VALUE read_clip(long);

/*
 * IFF code, adapted from the newiff/other/clipftxt.c example on the 2.0
 * native developer set.
 */

#define ID_FTXT  MAKE_ID('F','T','X','T')
#define ID_CHRS  MAKE_ID('C','H','R','S')

static const VALUE IFFErrorMsgs[] =
{
    MKSTR("End of file."),
    MKSTR("End of context."),
    MKSTR("No lexical scope."),
    MKSTR("No memory."),
    MKSTR("Stream read error."),
    MKSTR("Stream write error."),
    MKSTR("Stream seek error."),
    MKSTR("File is corrupt."),
    MKSTR("IFF syntax error."),
    MKSTR("Not an IFF file."),
    MKSTR("Required call-back hook missing."),
    MKSTR("Return to client.")
};

void *IFFParseBase;

void
clip_kill(void)
{
    if(IFFParseBase)
    {
	CloseLibrary(IFFParseBase);
	IFFParseBase = NULL;
    }
}

/*
 */
bool
write_clip(long unit, u_char *str, long strLen)
{
    if(IFFParseBase || (IFFParseBase = OpenLibrary("iffparse.library", 36)))
    {
	struct IFFHandle *iff;
	long error = 0;
	if(iff = AllocIFF())
	{
	    if(iff->iff_Stream = (ULONG)OpenClipboard(unit))
	    {
		InitIFFasClip(iff);
		if(!(error = OpenIFF(iff, IFFF_WRITE)))
		{
		    if(!(error = PushChunk(iff, ID_FTXT, ID_FORM, IFFSIZE_UNKNOWN)))
		    {
			if(!(error = PushChunk(iff, 0, ID_CHRS, IFFSIZE_UNKNOWN)))
			{
			    if(WriteChunkBytes(iff, str, strLen) != strLen)
				error = IFFERR_WRITE;
			    if(!error)
				error = PopChunk(iff);
			    else
				PopChunk(iff);
			}
			if(!error)
			    error = PopChunk(iff);
			else
			    PopChunk(iff);
		    }
		    CloseIFF(iff);
		}
		CloseClipboard((struct ClipboardHandle *)iff->iff_Stream);
	    }
	    else
		error = IFFERR_NOMEM;	/* ?? */
	    FreeIFF(iff);
	}
	else
	    error = IFFERR_NOMEM;

	CloseLibrary(IFFParseBase);
	IFFParseBase = NULL;

	if(error)
	{
	    cmd_signal(sym_error, list_2(MKSTR("iffparse"), IFFErrorMsgs[-error - 1]));
	    return(FALSE);
	}
	return(TRUE);
    }
    cmd_signal(sym_error, LIST_1(MKSTR("Can't open iffparse.library")));
    return(FALSE);
}

/*
 * note:
 *  Currently this only reads the first CHRS chunk that it finds.
 */
VALUE
read_clip(long unit)
{
    VALUE text = NULL;
    if(IFFParseBase || (IFFParseBase = OpenLibrary("iffparse.library", 36)))
    {
	struct IFFHandle *iff;
	long error;
	if(iff = AllocIFF())
	{
	    if(iff->iff_Stream = (ULONG)OpenClipboard(unit))
	    {
		InitIFFasClip(iff);
		if(!(error = OpenIFF(iff, IFFF_READ)))
		{
		    if(!(error = StopChunk(iff, ID_FTXT, ID_CHRS)))
		    {
			struct ContextNode *cn;
			if(!(error = ParseIFF(iff, IFFPARSE_SCAN)))
			{
			    cn = CurrentChunk(iff);
			    if(cn && (cn->cn_Type == ID_FTXT) && (cn->cn_ID == ID_CHRS))
			    {
				if(text = make_string(cn->cn_Size + 1))
				{
				    error = ReadChunkBytes(iff, VSTR(text), cn->cn_Size);
				    if(error > 0)
				    {
					VSTR(text)[error] = 0;
					error = 0;
				    }
				}
				else
				    error = IFFERR_NOMEM;
			    }
			    else
				error = IFFERR_NOTIFF;	/* ?? */
			}
		    }
		    CloseIFF(iff);
		}
		CloseClipboard((struct ClipboardHandle *)iff->iff_Stream);
	    }
	    else
		error = IFFERR_NOMEM;
	    FreeIFF(iff);
	}
	else
	    error = IFFERR_NOMEM;
	if(error)
	{
	    text = NULL;
	    cmd_signal(sym_error, list_2(MKSTR("iffparse"), IFFErrorMsgs[-error - 1]));
	}
	CloseLibrary(IFFParseBase);
	IFFParseBase = NULL;
    }
    else
	cmd_signal(sym_error, LIST_1(MKSTR("Can't open iffparse.library")));
    return(text);
}
