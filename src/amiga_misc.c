/* amiga_misc.c -- Miscellaneous functions for AmigaDOS
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
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include "jade_protos.h"

#include <clib/dos_protos.h>
#define INTUI_V36_NAMES_ONLY
#include <clib/intuition_protos.h>
#include <clib/asl_protos.h>
#include <exec/initializers.h>
#include <string.h>
#include <stdlib.h>

_PR void freq_kill(void);
_PR void beep(VW *);
_PR bool same_files(u_char *, u_char *);
_PR u_char *file_part(u_char *);
_PR void NewList(struct List *);
_PR VALUE lookup_errno(void);
_PR void doconmsg(u_char *);
_PR VALUE read_file(u_char *);

_PR bool file_exists(u_char *);
_PR long file_mod_time(u_char *);
_PR long sys_time(void);
_PR int add_file_part(u_char *, u_char *, int);
_PR VALUE sys_expand_file_name(VALUE);
_PR VALUE sys_fully_qualify_file_name(VALUE);

_PR void sys_misc_init(void);

/*
 * File req stuff.
 * I don't like the ASL requester but I couldn't be bothered to make the DICE
 * libraries for reqtools.library. Use the RTpatch prog to get nice (fast)
 * requesters.
 */
void *AslBase;
static struct FileRequester *file_req;

void
freq_kill(void)
{
    if(file_req)
    {
	FreeAslRequest(file_req);
	file_req = NULL;
    }
    if(AslBase)
    {
	CloseLibrary(AslBase);
	AslBase = NULL;
    }
}

_PR VALUE cmd_file_req(VALUE title, VALUE filename, VALUE writep);
DEFUN("file-req", cmd_file_req, subr_file_req, (VALUE title, VALUE filename, VALUE writep), V_Subr3, DOC_file_req) /*
::doc:file_req::
file-req TITLE [FILE-NAME] [FOR-WRITING-P]
<AMIGA ONLY>

Displays a file requester (standard one from asl.library) asking for the name
of a file. FOR-WRITING-P should be non-nil if the file being requested for
will be written to. TITLE is the name of the requester. FILE-NAME is
the starting value for the filename.

If a filename is selected its name is returned (a string), else this
function returns nil.
::end:: */
{
    VALUE result = sym_nil;
    DECLARE1(title, STRINGP);
    if(!STRINGP(filename))
	filename = null_string;
    if(AslBase || (AslBase = OpenLibrary("asl.library", 36)))
    {
	if(file_req
	   || (file_req = AllocAslRequestTags(ASL_FileRequest, TAG_DONE)))
	{
	    u_short flags = 0;
	    u_char *dircopy;
	    if(!NILP(writep))
		flags |= FILF_SAVE;
	    if(dircopy = str_dup(VSTR(filename)))
	    {
		u_char *actualfile = FilePart(VSTR(filename));
		dircopy[actualfile - VSTR(filename)] = 0;
		if(AslRequestTags(file_req,
		    ASL_Hail, VSTR(title),
		    ASL_Window, curr_vw->vw_Window,
		    ASL_File, actualfile,
		    ASL_Dir, dircopy,
		    ASL_FuncFlags, flags,
		    TAG_END))
		{
		    long sellen = strlen(file_req->rf_File) + strlen(file_req->rf_Dir) + 2;
		    if(result = make_string(sellen))
		    {
			strcpy(VSTR(result), file_req->rf_Dir);
			AddPart(VSTR(result), file_req->rf_File, sellen);
			/* Ensure that the length field contains the *correct*
			   value.  */
			DSTRING_HDR(result)->ds_Length = strlen(VSTR(result));
		    }
		    else
			mem_error();
		}
		str_free(dircopy);
	    }
	    else
		mem_error();
	}
	else
	    cmd_signal(sym_error, LIST_1(MKSTR("Can't allocate file requester")));
    }
    else
	cmd_signal(sym_error, LIST_1(MKSTR("Need `asl.library'")));
    return(result);
}

void
beep(VW *vw)
{
    DisplayBeep(vw->vw_Window->WScreen);
}

bool
same_files(u_char *file1, u_char *file2)
{
    bool rc = FALSE;
    BPTR lck1;
    if(lck1 = Lock(file1, SHARED_LOCK))
    {
	BPTR lck2;
	if(lck2 = Lock(file2, SHARED_LOCK))
	{
	    if(SameLock(lck1, lck2) == LOCK_SAME)
		rc = TRUE;
	    UnLock(lck2);
	}
	UnLock(lck1);
    }
    else
	rc = !stricmp(file1, file2);
    return(rc);
}

u_char *
file_part(u_char *file)
{
    return(FilePart(file));
}

#ifdef _DCC
void
NewList(struct List *list)
{
    list->lh_Head = (struct Node *)&list->lh_Tail;
    list->lh_Tail = NULL;
    list->lh_TailPred = (struct Node *)&list->lh_Head;
}
#endif

VALUE
lookup_errno(void)
{
    u_char buf[256];
    if(!Fault(IoErr(), "", buf, 256))
	sprintf(buf, "%d", IoErr());
    return(string_dup(buf));
}

void
doconmsg(u_char *msg)
{
    /*
     * CLI/WB & 1.3/2.0 compatible
     */
    if(ami_from_wb)
    {
	BPTR fh = Open("CON:///80/Jade output/WAIT/CLOSE", MODE_NEWFILE);
	if(fh)
	    Write(fh, msg, strlen(msg));
	Close(fh);
    }
    else
	Write(Output(), msg, strlen(msg));
}

VALUE
read_file(u_char *fileName)
{
    BPTR fh = Open(fileName, MODE_OLDFILE);
    if(fh)
    {
	long length;
	VALUE mem;
	Seek(fh, 0, OFFSET_END);
	length = Seek(fh, 0, OFFSET_BEGINNING);
	if(mem = make_string(length + 1))
	{
	    Read(fh, VSTR(mem), length);
	    VSTR(mem)[length] = 0;
	    Close(fh);
	    return(mem);
	}
	else
	    mem_error();
	Close(fh);
    }
    return(cmd_signal(sym_file_error, list_2(lookup_errno(), string_dup(fileName))));
}

int
add_file_part(u_char *buf, u_char *part, int len)
{
    return(AddPart(buf, part, len));
}

VALUE
sys_expand_file_name(VALUE name)
{
    return(name);
}

VALUE
sys_fully_qualify_file_name(VALUE name)
{
    u_char buf[512];
    BPTR lock = Lock(VSTR(name), ACCESS_READ);
    if(lock)
    {
	/* easy, just expand from the lock. */
	if(NameFromLock(lock, buf, 512))
	    name = string_dup(buf);
	else
	    name = NULL;
    }
    else
    {
	/* trickier, the file doesn't exist; find its parent and work from
	   there. */
	long len = PathPart(VSTR(name)) - VSTR(name);
	memcpy(buf, VSTR(name), len);
	buf[len] = 0;
	lock = Lock(buf, ACCESS_READ);
	if(lock && NameFromLock(lock, buf, 512))
	{
	    if(add_file_part(buf, VSTR(name) + len, 512))
		name = string_dup(buf);
	    else
		name = NULL;
	}
    }
    if(lock != NULL)
	UnLock(lock);
    return(name);
}

long
getfibfield(u_char *file, int field)
{
    long rc = 0;
    BPTR lock;
    if(lock = Lock(file, SHARED_LOCK))
    {
	struct FileInfoBlock *fib;
	if(fib = mymalloc(sizeof(struct FileInfoBlock)))
	{
	    if(Examine(lock, fib))
		rc = *(LONG *)((char *)fib + field);
	    myfree(fib);
	}
	UnLock(lock);
    }
    return(rc);
}

_PR VALUE cmd_flush_output(void);
DEFUN("flush-output", cmd_flush_output, subr_flush_output, (void), V_Subr0, DOC_flush_output)
{
    return(sym_t);
}

_PR VALUE cmd_delete_file(VALUE file);
DEFUN_INT("delete-file", cmd_delete_file, subr_delete_file, (VALUE file), V_Subr1, DOC_delete_file, "fDelete file:")
{
    DECLARE1(file, STRINGP);
    if(DeleteFile(VSTR(file)))
	return(sym_t);
    return(cmd_signal(sym_file_error, list_2(lookup_errno(), file)));
}

_PR VALUE cmd_rename_file(VALUE src, VALUE dst);
DEFUN_INT("rename-file", cmd_rename_file, subr_rename_file, (VALUE src, VALUE dst), V_Subr2, DOC_rename_file, "fRename file:\nFRename file `%s' as:")
{
    DECLARE1(src, STRINGP);
    DECLARE2(dst, STRINGP);
    if(Rename(VSTR(src), VSTR(dst)))
	return(sym_t);
    return(cmd_signal(sym_file_error, list_3(lookup_errno(), src, dst)));
}

_PR VALUE cmd_copy_file(VALUE src, VALUE dst);
DEFUN_INT("copy-file", cmd_copy_file, subr_copy_file, (VALUE src, VALUE dst), V_Subr2, DOC_copy_file, "fCopy file:\nFCopy file `%s' to:")
{
    VALUE res = sym_t;
    BPTR srcf;
    DECLARE1(src, STRINGP);
    DECLARE2(dst, STRINGP);
    srcf = Open(VSTR(src), MODE_OLDFILE);
    if(srcf != NULL)
    {
	int dstf = Open(VSTR(dst), MODE_NEWFILE);
	if(dstf != NULL)
	{
	    int rd;
	    int prot = getfibfield(VSTR(src), (int)OFFSET(FileInfoBlock, fib_Protection));
	    if(prot != 0)
		SetProtection(VSTR(dst), prot &~ FIBF_ARCHIVE);
	    do {
		u_char buf[BUFSIZ];
		int wr;
		rd = Read(srcf, buf, BUFSIZ);
		if(rd < 0)
		{
		    res = signal_file_error(src);
		    break;
		}
		wr = Write(dstf, buf, rd);
		if(wr != rd)
		{
		    res = signal_file_error(dst);
		    break;
		}
	    } while(rd != 0);
	    Close(dstf);
	}
	else
	    res = signal_file_error(dst);
	Close(srcf);
    }
    else
	res = signal_file_error(src);
    return(res);
}

_PR VALUE cmd_file_readable_p(VALUE file);
DEFUN("file-readable-p", cmd_file_readable_p, subr_file_readable_p, (VALUE file), V_Subr1, DOC_file_readable_p)
{
    int prot;
    DECLARE1(file, STRINGP);
    prot= getfibfield(VSTR(file), (int)OFFSET(FileInfoBlock, fib_Protection));
    if(~prot & FIBF_READ)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_file_writable_p(VALUE file);
DEFUN("file-writable-p", cmd_file_writable_p, subr_file_writable_p, (VALUE file), V_Subr1, DOC_file_writeable_p)
{
    int prot;
    DECLARE1(file, STRINGP);
    prot = getfibfield(VSTR(file), (int)OFFSET(FileInfoBlock, fib_Protection));
    if(~prot & FIBF_WRITE)
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_file_exists_p(VALUE file);
DEFUN("file-exists-p", cmd_file_exists_p, subr_file_exists_p, (VALUE file), V_Subr1, DOC_file_exists_p)
{
    BPTR lock;
    DECLARE1(file, STRINGP);
    if(lock = Lock(VSTR(file), SHARED_LOCK))
    {
	UnLock(lock);
	return(sym_t);
    }
    return(sym_nil);
}

bool
file_exists(u_char *fileName)
{
    BPTR lock;
    if(lock = Lock(fileName, SHARED_LOCK))
    {
	int type;
	UnLock(lock);
	type = getfibfield(fileName, (int)OFFSET(FileInfoBlock, fib_DirEntryType));
	/* Only allow _files_ */
	if(type < 0)
	    return(TRUE);
    }
    return(FALSE);
}

_PR VALUE cmd_file_regular_p(VALUE file);
DEFUN("file-regular-p", cmd_file_regular_p, subr_file_regular_p, (VALUE file), V_Subr1, DOC_file_regular_p)
{
    int type;
    DECLARE1(file, STRINGP);
    if(type = getfibfield(VSTR(file), (int)OFFSET(FileInfoBlock, fib_DirEntryType)))
    {
	if(type < 0)
	    return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_file_directory_p(VALUE file);
DEFUN("file-directory-p", cmd_file_directory_p, subr_file_directory_p, (VALUE file), V_Subr1, DOC_file_directory_p)
{
    int type;
    DECLARE1(file, STRINGP);
    if(type = getfibfield(VSTR(file), (int)OFFSET(FileInfoBlock, fib_DirEntryType)))
    {
	/* There may be a problem here, ST_SOFTLINK>0 so, these will make
	   this function return t.  */
	if(type > 0)
	    return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_file_symlink_p(VALUE file);
DEFUN("file-symlink-p", cmd_file_symlink_p, subr_file_symlink_p, (VALUE file), V_Subr1, DOC_file_symlink_p)
{
    int type;
    DECLARE1(file, STRINGP);
    if(type = getfibfield(VSTR(file), (int)OFFSET(FileInfoBlock, fib_DirEntryType)))
    {
	if(type == ST_SOFTLINK)
	    return(sym_t);
    }
    return(sym_nil);
}

_PR VALUE cmd_file_owner_p(VALUE file);
DEFUN("file-owner-p", cmd_file_owner_p, subr_file_owner_p, (VALUE file), V_Subr1, DOC_file_owner_p)
{
    return(sym_t);
}

_PR VALUE cmd_file_nlinks(VALUE file);
DEFUN("file-nlinks", cmd_file_nlinks, subr_file_nlinks, (VALUE file), V_Subr1, DOC_file_nlinks)
{
    /* I don't know how to do this in AmigaDOS -- is it possible? */
    return(make_number(1));
}

_PR VALUE cmd_file_modes(VALUE file);
DEFUN("file-modes", cmd_file_modes, subr_file_modes, (VALUE file), V_Subr1, DOC_file_modes)
{
    DECLARE1(file, STRINGP);
    int bits = getfibfield(VSTR(file), (int)OFFSET(FileInfoBlock, fib_Protection));
    if(bits != 0)
	return(make_number(bits));
    return(sym_nil);
}

_PR VALUE cmd_set_file_modes(VALUE file, VALUE modes);
DEFUN("set-file-modes", cmd_set_file_modes, subr_set_file_modes, (VALUE file, VALUE modes), V_Subr2, DOC_set_file_modes)
{
    DECLARE1(file, STRINGP);
    DECLARE2(modes, NUMBERP);
    if(SetProtection(VSTR(file), VNUM(modes) & ~FIBF_ARCHIVE))
	return(sym_t);
    return(signal_file_error(file));
}

static long
dstotime(struct DateStamp *ds)
{
    return((ds->ds_Days * 86400) + (ds->ds_Minute * 60) + (ds->ds_Tick / TICKS_PER_SECOND));
}

long
file_mod_time(u_char *fileName)
{
    long time = 0;
    BPTR lock;
    if(lock = Lock(fileName, SHARED_LOCK))
    {
	struct FileInfoBlock *fib;
	if(fib = mymalloc(sizeof(struct FileInfoBlock)))
	{
	    if(Examine(lock, fib))
		time = dstotime(&fib->fib_Date);
	    myfree(fib);
	}
	UnLock(lock);
    }
    return(time);
}

long
sys_time(void)
{
    struct DateStamp ds;
    DateStamp(&ds);
    return(dstotime(&ds));
}

_PR VALUE cmd_file_modtime(VALUE file);
DEFUN("file-modtime", cmd_file_modtime, subr_file_modtime, (VALUE file), V_Subr1, DOC_file_modtime)
{
    DECLARE1(file, STRINGP);
    return(make_number(file_mod_time(VSTR(file))));
}

_PR VALUE cmd_directory_files(VALUE dir);
DEFUN("directory-files", cmd_directory_files, subr_directory_files, (VALUE dir), V_Subr1, DOC_directory_files)
{
    BPTR dirlock;
    DECLARE1(dir, STRINGP);
    if(dirlock = Lock(VSTR(dir), SHARED_LOCK))
    {
	VALUE list = sym_nil;
	struct FileInfoBlock *fib;
	if(fib = mymalloc(sizeof(struct FileInfoBlock)))
	{
	    if(Examine(dirlock, fib))
	    {
		while(ExNext(dirlock, fib))
		{
		    VALUE name;
		    if(!((name = string_dup(fib->fib_FileName)) && (list = cmd_cons(name, list))))
		    {
			myfree(fib);
			UnLock(dirlock);
			return(mem_error());
		    }
		}
	    }
	    myfree(fib);
	}
	UnLock(dirlock);
	return(list);
    }
    return(cmd_signal(sym_file_error, list_2(lookup_errno(), dir)));
}

_PR VALUE cmd_user_login_name(void);
DEFUN("user-login-name", cmd_user_login_name, subr_user_login_name, (void), V_Subr0, DOC_user_login_name)
{
    char *name;
    /* Just look this up once, then use the saved copy.  */
    static VALUE user_login_name;
    if(user_login_name)
	return(user_login_name);
    name = getenv("USERNAME");
    if(name)
	user_login_name = string_dup(name);
    else
	user_login_name = MKSTR("<unknown username>");
    mark_static(&user_login_name);
    return(user_login_name);
}

_PR VALUE cmd_user_full_name(void);
DEFUN("user-full-name", cmd_user_full_name, subr_user_full_name, (void), V_Subr0, DOC_user_full_name)
{
    char *name;
    static VALUE user_full_name;
    if(user_full_name)
	return(user_full_name);
    name = getenv("REALNAME");
    if(name)
	user_full_name = string_dup(name);
    else
	user_full_name = MKSTR("<unknown full name>");
    mark_static(&user_full_name);
    return(user_full_name);
}

_PR VALUE cmd_user_home_directory(void);
DEFUN("user-home-directory", cmd_user_home_directory, subr_user_home_directory, (void), V_Subr0, DOC_user_home_directory)
{
    char *home;
    static VALUE user_home_directory;
    if(user_home_directory)
	return(user_home_directory);
    home = getenv("HOME");
    if(home)
	user_home_directory = string_dup(home);
    else
	user_home_directory = MKSTR("SYS:");
    mark_static(&user_home_directory);
    return(user_home_directory);
}

_PR VALUE cmd_system_name(void);
DEFUN("system-name", cmd_system_name, subr_system_name, (void), V_Subr0,  DOC_system_name)
{
    char *name;
    static VALUE system_name;
    if(system_name)
	return(system_name);
    name = getenv("HOSTNAME");
    if(name)
	system_name = string_dup(name);
    else
	system_name = MKSTR("<unknown system name>");
    mark_static(&system_name);
    return(system_name);
}

_PR VALUE cmd_setenv(VALUE name, VALUE val);
DEFUN("setenv", cmd_setenv, subr_setenv, (VALUE name, VALUE val), V_Subr2, DOC_setenv)
{
    DECLARE1(name, STRINGP);
    if(!STRINGP(val))
    {
	/* deleting the variable NAME */
	return(DeleteVar(VSTR(name), GVF_GLOBAL_ONLY) ? sym_t : sym_nil);
    }
    else
    {
	/* setting NAME to VAL */
	if(SetVar(VSTR(name), VSTR(val), STRING_LEN(val), GVF_GLOBAL_ONLY))
	    return(val);
	else
	    return(cmd_signal(sym_file_error,
			      list_2(MKSTR("Can't set env variable"), name)));
    }
}

void
sys_misc_init(void)
{
    ADD_SUBR(subr_file_req);
    ADD_SUBR(subr_flush_output);
    ADD_SUBR(subr_delete_file);
    ADD_SUBR(subr_rename_file);
    ADD_SUBR(subr_copy_file);
    ADD_SUBR(subr_file_readable_p);
    ADD_SUBR(subr_file_writable_p);
    ADD_SUBR(subr_file_exists_p);
    ADD_SUBR(subr_file_regular_p);
    ADD_SUBR(subr_file_directory_p);
    ADD_SUBR(subr_file_symlink_p);
    ADD_SUBR(subr_file_owner_p);
    ADD_SUBR(subr_file_nlinks);
    ADD_SUBR(subr_file_modes);
    ADD_SUBR(subr_set_file_modes);
    ADD_SUBR(subr_file_modtime);
    ADD_SUBR(subr_directory_files);
    ADD_SUBR(subr_user_login_name);
    ADD_SUBR(subr_user_full_name);
    ADD_SUBR(subr_user_home_directory);
    ADD_SUBR(subr_system_name);
    ADD_SUBR(subr_setenv);
}
