/* faces.c -- faces and colors -- How to render glyphs
   Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
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
#include <lib/jade_protos.h>
#include <string.h>

_PR void face_prin(VALUE strm, VALUE obj);
_PR void face_sweep(void);
_PR void mark_faces(void);
_PR void color_prin(VALUE strm, VALUE obj);
_PR void color_sweep(void);
_PR void faces_init(void);


/* faces */

_PR Lisp_Face *allocated_faces, *face_table[];
Lisp_Face *allocated_faces;
Lisp_Face *face_table[GA_LastFace+1];

DEFSYM(foreground, "foreground");
DEFSYM(background, "background");
DEFSYM(underline, "underline");
DEFSYM(bold, "bold");
DEFSYM(italic, "italic");
_PR VALUE sym_foreground, sym_background, sym_underline, sym_bold, sym_italic;

DEFSYM(default_face, "default-face");
DEFSYM(block_face, "block-face");
DEFSYM(modeline_face, "modeline-face");
DEFSYM(highlight_face, "highlight-face");
DEFSYM(face, "face");
_PR VALUE sym_default_face, sym_block_face, sym_modeline_face;
_PR VALUE sym_highlight_face, sym_face;

char *default_fg_color = "black";
char *default_bg_color = "#cdcdc1";
char *default_block_color = "lightblue";
char *default_hl_color = "lightgoldenrod";
char *default_ml_color = "lightsteelblue";
_PR char *default_fg_color, *default_bg_color;
_PR char *default_block_color, *default_hl_color, *default_ml_color;

_PR VALUE cmd_make_face(VALUE, VALUE);
DEFUN("make-face", cmd_make_face, subr_make_face, (VALUE name, VALUE src),
      V_Subr2, DOC_make_face) /*
::doc:make_face::
make-face NAME [SOURCE]

Return the face called NAME. If no face with this name exists already, create
a new one, its initial state will be copied from SOURCE, or the default-face
if SOURCE is not defined.
::end:: */
{
    Lisp_Face *f;
    int id;

    DECLARE1(name, STRINGP);

    for(f = allocated_faces; f != 0; f = f->next)
    {
	if(strcmp(VSTR(f->name), VSTR(name)) == 0)
	    return VAL(f);
    }

    if(!FACEP(src))
	src = cmd_symbol_value(sym_default_face, sym_t);

    for(id = GA_FirstFace; id <= GA_LastFace && face_table[id] != 0; id++) ;
    if(id >= GA_LastFace)
	return mem_error();		/* !? */

    f = ALLOC_OBJECT(sizeof(Lisp_Face));
    if(f == 0)
	return mem_error();
    f->next = allocated_faces;
    allocated_faces = f;
    f->id = id;
    f->name = name;

    if(FACEP(src))
    {
	f->car = VFACE(src)->car;
	f->foreground = VFACE(src)->foreground;
	f->background = VFACE(src)->background;
    }
    else
    {
	f->car = V_Face;
	f->foreground = f->background = sym_nil;
    }

    face_table[id] = f;
    return VAL(f);
}

_PR VALUE cmd_delete_face(VALUE);
DEFUN("delete-face", cmd_delete_face, subr_delete_face, (VALUE face),
      V_Subr1, DOC_delete_face) /*
::doc:delete_face::
delete-face FACE

Mark that FACE's slot in the table of faces may be reused. FACE will no
longer be displayable.
::end:: */
{
    DECLARE1(face, FACEP);
    face_table[VFACE(face)->id] = 0;
    VFACE(face)->id = 0;
    return face;
}

void
face_prin(VALUE strm, VALUE obj)
{
    char buf[128];
#ifdef HAVE_SNPRINT
    snprintf
#else
    sprintf
#endif
    (buf, "#<face %s>", VSTR(VFACE(obj)->name));
    stream_puts(strm, buf, -1, FALSE);
}

void
face_sweep(void)
{
    Lisp_Face *f = allocated_faces;
    allocated_faces = 0;
    while(f != 0)
    {
	Lisp_Face *next = f->next;
	if(!GC_CELL_MARKEDP(VAL(f)))
	    FREE_OBJECT(f);
	else
	{
	    GC_CLR_CELL(VAL(f));
	    f->next = allocated_faces;
	    allocated_faces = f;
	}
	f = next;
    }
}

void
mark_faces(void)
{
    int i;
    for(i = GA_FirstFace; i <= GA_LastFace; i++)
    {
	if(face_table[i] != 0)
	    MARKVAL(VAL(face_table[i]));
    }
}

_PR VALUE cmd_set_face_attribute(VALUE, VALUE, VALUE);
DEFUN("set-face-attribute", cmd_set_face_attribute, subr_set_face_attribute,
      (VALUE face, VALUE attr, VALUE value), V_Subr3,
      DOC_set_face_attribute) /*
::doc:set_face_attribute::
set-face-attribute FACE ATTRIBUTE-NAME VALUE

Set the attribute named ATTRIBUTE-NAME of FACE to VALUE. ATTRIBUTE-NAME
may be one of these symbols:

  `foreground', `background'
	The colours used for glyphs and blank space respectably. Note
	that VALUE may be either the color object, or the name of the
	color.

  `underline', 'bold', `italic'
	Boolean attributes.
::end:: */
{
    WIN *w;
    DECLARE1(face, FACEP);

    if(attr == sym_foreground || attr == sym_background)
    {
	if(STRINGP(value))
	    value = cmd_get_color(value);
	DECLARE3(value, COLORP);
	if(attr == sym_foreground)
	    VFACE(face)->foreground = value;
	else
	    VFACE(face)->background = value;
    }
    else if(attr == sym_underline)
    {
	VFACE(face)->car &= ~FACEFF_UNDERLINE;
	if(!NILP(value))
	    VFACE(face)->car |= FACEFF_UNDERLINE;
    }
    else if(attr == sym_bold)
    {
	VFACE(face)->car &= ~FACEFF_BOLD;
	if(!NILP(value))
	    VFACE(face)->car |= FACEFF_BOLD;
    }
    else if(attr == sym_italic)
    {
	VFACE(face)->car &= ~FACEFF_ITALIC;
	if(!NILP(value))
	    VFACE(face)->car |= FACEFF_ITALIC;
    }
    else
	return signal_arg_error(attr, 2);

    for(w = win_chain; w != 0; w = w->w_Next)
    {
	if(w->w_Window)
	{
	    int row, fid = VFACE(face)->id;
	    glyph_buf *g = w->w_Content;
	    for(row = 0; row < g->rows; row++)
	    {
		glyph_attr *attrs = g->attrs[row];
		bool modified = FALSE;
		int col;
		for(col = 0; col < g->cols; col++)
		{
		    if((attrs[col] & ~GA_CursorFace) == fid)
		    {
			attrs[col] = GA_Garbage;
			modified = TRUE;
		    }
		}
		if(modified)
		    g->hashes[row] = hash_glyph_row(g, row);
	    }
	}
    }

    return value;
}

_PR VALUE cmd_face_attribute(VALUE, VALUE);
DEFUN("face-attribute", cmd_face_attribute, subr_face_attribute,
      (VALUE face, VALUE attr), V_Subr2, DOC_face_attribute) /*
::doc:face_attribute::
face-attribute FACE ATTRIBUTE-NAME

Return the current value of the attribute named ATTRIBUTE-NAME in FACE.
See `set-face-attribute' for the list of possible attributes.
::end:: */
{
    DECLARE1(face, FACEP);

    if(attr == sym_foreground)
	return VFACE(face)->foreground;
    else if(attr == sym_background)
	return VFACE(face)->background;
    else if(attr == sym_underline)
	return (VFACE(face)->car & FACEFF_UNDERLINE) ? sym_t : sym_nil;
    else if(attr == sym_bold)
	return (VFACE(face)->car & FACEFF_BOLD) ? sym_t : sym_nil;
    else if(attr == sym_italic)
	return (VFACE(face)->car & FACEFF_ITALIC) ? sym_t : sym_nil;
    else
	return signal_arg_error(attr, 2);
}

_PR VALUE cmd_face_id(VALUE);
DEFUN("face-id", cmd_face_id, subr_face_id, (VALUE face),
      V_Subr1, DOC_face_id) /*
::doc:face_id::
face-id FACE

Return the id of the slot in the face table used by FACE. Returns nil
if this face has been deleted but not garbage collected.
::end:: */
{
    DECLARE1(face, FACEP);

    return VFACE(face)->id != 0 ? MAKE_INT(VFACE(face)->id) : sym_nil;
}


/* colors */

_PR Lisp_Color *allocated_colors;
Lisp_Color *allocated_colors;

_PR VALUE cmd_get_color(VALUE);
DEFUN("get-color", cmd_get_color, subr_get_color, (VALUE name), V_Subr1,
      DOC_get_color) /*
::doc:get_color::
get-color NAME

Return the Lisp object defining the color named NAME (a string).
::end:: */
{
    Lisp_Color *c;
    DECLARE1(name, STRINGP);

    for(c = allocated_colors; c != 0; c = c->next)
    {
	if(strcasecmp(VSTR(c->name), VSTR(name)) == 0)
	    return VAL(c);
    }

    c = ALLOC_OBJECT(sizeof(Lisp_Color));
    if(c == 0)
	return mem_error();
    c->car = V_Color;
    c->next = allocated_colors;
    allocated_colors = c;
    c->name = name;

    return sys_make_color(c);
}

void
color_prin(VALUE strm, VALUE obj)
{
    char buf[128];
#ifdef HAVE_SNPRINT
    snprintf
#else
    sprintf
#endif
    (buf, "#<color %s>", VSTR(VCOLOR(obj)->name));
    stream_puts(strm, buf, -1, FALSE);
}

void
color_sweep(void)
{
    Lisp_Color *c = allocated_colors;
    allocated_colors = 0;
    while(c != 0)
    {
	Lisp_Color *next = c->next;
	if(!GC_CELL_MARKEDP(VAL(c)))
	{
	    sys_free_color(c);
	    FREE_OBJECT(c);
	}
	else
	{
	    GC_CLR_CELL(VAL(c));
	    c->next = allocated_colors;
	    allocated_colors = c;
	}
	c = next;
    }
}



void
faces_init(void)
{
    VALUE face, fg, bg, bl, hl, ml;

    ADD_SUBR(subr_make_face);
    ADD_SUBR(subr_delete_face);
    ADD_SUBR(subr_set_face_attribute);
    ADD_SUBR(subr_face_attribute);
    ADD_SUBR(subr_face_id);
    ADD_SUBR(subr_get_color);

    INTERN(foreground);
    INTERN(background);
    INTERN(underline);
    INTERN(bold);
    INTERN(italic);
    INTERN(default_face);
    INTERN(block_face);
    INTERN(modeline_face);
    INTERN(highlight_face);
    INTERN(face);

    fg = cmd_get_color(string_dup(default_fg_color));
    bg = cmd_get_color(string_dup(default_bg_color));
    bl = cmd_get_color(string_dup(default_block_color));
    hl = cmd_get_color(string_dup(default_hl_color));
    ml = cmd_get_color(string_dup(default_ml_color));

    face = cmd_make_face(VSYM(sym_default_face)->name, sym_nil);
    cmd_set_face_attribute(face, sym_foreground, fg);
    cmd_set_face_attribute(face, sym_background, bg);
    VSYM(sym_default_face)->value = face;

    face = cmd_make_face(VSYM(sym_block_face)->name, sym_nil);
    cmd_set_face_attribute(face, sym_foreground, fg);
    cmd_set_face_attribute(face, sym_background, bl);
    VSYM(sym_block_face)->value = face;

    face = cmd_make_face(VSYM(sym_modeline_face)->name, sym_nil);
    cmd_set_face_attribute(face, sym_foreground, fg);
    cmd_set_face_attribute(face, sym_background, ml);
    VSYM(sym_modeline_face)->value = face;

    face = cmd_make_face(VSYM(sym_highlight_face)->name, sym_nil);
    cmd_set_face_attribute(face, sym_foreground, fg);
    cmd_set_face_attribute(face, sym_background, hl);
    VSYM(sym_highlight_face)->value = face;
}
