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
#include <assert.h>

_PR void face_prin(VALUE strm, VALUE obj);
_PR void face_sweep(void);
_PR int merge_faces(WIN *w, Lisp_Extent *e, int in_active, int on_cursor);
_PR int get_face_id(WIN *w, Lisp_Face *f);
_PR void mark_merged_faces(WIN *w);
_PR void color_prin(VALUE strm, VALUE obj);
_PR void color_sweep(void);
_PR bool faces_init(void);


/* faces */

_PR Lisp_Face *allocated_faces;
Lisp_Face *allocated_faces;

DEFSYM(foreground, "foreground");
DEFSYM(background, "background");
DEFSYM(underline, "underline");
DEFSYM(bold, "bold");
DEFSYM(italic, "italic");
DEFSYM(inverted, "inverted");
DEFSYM(boxed, "boxed");
_PR VALUE sym_foreground, sym_background, sym_underline, sym_bold, sym_italic;
_PR VALUE sym_inverted, sym_boxed;

DEFSYM(default_face, "default-face");
DEFSYM(block_face, "block-face");
DEFSYM(modeline_face, "modeline-face");
DEFSYM(highlight_face, "highlight-face");
DEFSYM(face, "face");
_PR VALUE sym_default_face, sym_block_face, sym_modeline_face;
_PR VALUE sym_highlight_face, sym_face;

_PR VALUE mouse_cursor_face;
VALUE mouse_cursor_face;

char *default_fg_color = "black";
char *default_bg_color = "#cdcdc1";
char *default_block_color = "lightblue";
char *default_hl_color = "lightgoldenrod";
char *default_ml_color = "lightsteelblue";
_PR char *default_fg_color, *default_bg_color;
_PR char *default_block_color, *default_hl_color, *default_ml_color;

_PR VALUE cmd_make_face(VALUE);
DEFUN("make-face", cmd_make_face, subr_make_face, (VALUE name),
      V_Subr1, DOC_make_face) /*
::doc:make_face::
make-face NAME

Return the face called NAME. If no face with this name exists already, create
a new one, its initial state will be to have absolutely no effect.
::end:: */
{
    Lisp_Face *f;

    DECLARE1(name, STRINGP);

    for(f = allocated_faces; f != 0; f = f->next)
    {
	if(strcmp(VSTR(f->name), VSTR(name)) == 0)
	    return VAL(f);
    }

    f = ALLOC_OBJECT(sizeof(Lisp_Face));
    if(f == 0)
	return mem_error();
    f->next = allocated_faces;
    allocated_faces = f;
    f->name = name;
    f->car = V_Face;
    f->foreground = f->background = sym_nil;

    return VAL(f);
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

  `underline', 'bold', `italic', `inverted', `boxed'
	Boolean attributes.
::end:: */
{
    WIN *w;
    DECLARE1(face, FACEP);

    if(attr == sym_foreground || attr == sym_background)
    {
	if(STRINGP(value))
	{
	    value = cmd_get_color(value);
	    if(!value)
		return value;
	}
	if(!NILP(value))
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
    else if(attr == sym_inverted)
    {
	VFACE(face)->car &= ~FACEFF_INVERT;
	if(!NILP(value))
	    VFACE(face)->car |= FACEFF_INVERT;
    }
    else if(attr == sym_boxed)
    {
	VFACE(face)->car &= ~FACEFF_BOXED;
	if(!NILP(value))
	    VFACE(face)->car |= FACEFF_BOXED;
    }
    else
	return signal_arg_error(attr, 2);

    for(w = win_chain; w != 0; w = w->w_Next)
    {
	if(w->w_Window)
	    /* TODO: this is highly suboptimal... */
	    w->w_Flags |= WINFF_FORCE_REFRESH;
    }
    if(face == mouse_cursor_face)
	sys_recolor_cursor(face);

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
    else if(attr == sym_inverted)
	return (VFACE(face)->car & FACEFF_INVERT) ? sym_t : sym_nil;
    else if(attr == sym_boxed)
	return (VFACE(face)->car & FACEFF_BOXED) ? sym_t : sym_nil;
    else
	return signal_arg_error(attr, 2);
}

_PR VALUE var_mouse_cursor_face(VALUE);
DEFUN("mouse-cursor-face", var_mouse_cursor_face, subr_mouse_cursor_face,
      (VALUE arg), V_Var, DOC_mouse_cursor_face) /*
::doc:mouse_cursor_face::
The face used to color the mouse cursor.
::end:: */
{
    if(arg)
    {
	mouse_cursor_face = arg;
	sys_recolor_cursor(arg);
    }
    return mouse_cursor_face;
}
    

/* rendering with faces */

static int
get_merged_face(WIN *w, u_short car,
		Lisp_Color *background, Lisp_Color *foreground)
{
    int id, empty = -1;

    /* Find a space in the MergedFaces table, either containing
       an equivalent of this one, or an empty slot. */
    for(id = 0; id <= GA_LastFace; id++)
    {
	if(w->w_MergedFaces[id].valid)
	{
	    Merged_Face *f = &w->w_MergedFaces[id];
	    if(f->car == car
	       && f->background == background
	       && f->foreground == foreground)
	    {
		return id;
	    }
	}
	else if(empty == -1)
	    empty = id;
    }
    assert(empty != -1);		/* FIXME: handle gracefully? */

    /* Fill the new face */
    w->w_MergedFaces[empty].car = car;
    w->w_MergedFaces[empty].valid = TRUE;
    w->w_MergedFaces[empty].background = background;
    w->w_MergedFaces[empty].foreground = foreground;
    return empty;
}

/* Return the id of a face in W->w_MergedFaces that expresses the
   attributes of the positions within E. */
int
merge_faces(WIN *w, Lisp_Extent *e, int in_block, int on_cursor)
{
    u_long car = 0;
    Lisp_Color *background = 0, *foreground = 0;

    Lisp_Extent *x;

    void union_face(Lisp_Face *face) {
	car |= face->car & (FACEFF_MASK & ~FACEFF_INVERT);
	if(face->car & FACEFF_INVERT)
	    car ^= FACEFF_INVERT;
	if(background == 0 && COLORP(face->background))
	    background = VCOLOR(face->background);
	if(foreground == 0 && COLORP(face->foreground))
	    foreground = VCOLOR(face->foreground);
    }

    if(on_cursor)
    {
	if(WINDOW_HAS_FOCUS(w))
	    car |= FACEFF_INVERT;
	else
	    car |= FACEFF_BOXED;
    }

    if(in_block)
    {
	VALUE face = VSYM(sym_block_face)->value;
	if(FACEP(face))
	    union_face(VFACE(face));
    }

    /* Work up from E to the root. */
    for(x = e; x != 0; x = x->parent)
    {
	VALUE face = cmd_extent_get(VAL(x), sym_face);
	if(FACEP(face))
	    union_face(VFACE(face));
    }

    /* Merge in the default-face properties */
    {
	VALUE face = VSYM(sym_default_face)->value;
	if(FACEP(face))
	    union_face(VFACE(face));
    }

    assert(background != 0 && foreground != 0);

    return get_merged_face(w, car, background, foreground);
}

int
get_face_id(WIN *w, Lisp_Face *f)
{
    assert(COLORP(f->background) && COLORP(f->foreground));
    return get_merged_face(w, f->car & FACEFF_MASK,
			   VCOLOR(f->background), VCOLOR(f->foreground));
}

static void
mark_glyph_buf_faces(WIN *w, glyph_buf *g)
{
    int row, col;
    int id;

    for(id = 0; id <= GA_LastFace; id++)
	w->w_MergedFaces[id].valid = FALSE;

    for(row = 0; row < g->rows; row++)
    {
	glyph_attr *attrs = g->attrs[row];
	for(col = 0; col < g->cols; col++)
	    w->w_MergedFaces[attrs[col]].valid = TRUE;
    }

    for(id = 0; id <= GA_LastFace; id++)
    {
	if(w->w_MergedFaces[id].valid)
	{
	    MARKVAL(VAL(w->w_MergedFaces[id].background));
	    MARKVAL(VAL(w->w_MergedFaces[id].foreground));
	}
    }
}

void
mark_merged_faces(WIN *w)
{
    mark_glyph_buf_faces(w, w->w_Content);
    mark_glyph_buf_faces(w, w->w_NewContent);
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



bool
faces_init(void)
{
    VALUE face, fg, bg, bl, hl, ml;

    ADD_SUBR(subr_make_face);
    ADD_SUBR(subr_set_face_attribute);
    ADD_SUBR(subr_face_attribute);
    ADD_SUBR(subr_mouse_cursor_face);
    mark_static(&mouse_cursor_face);
    ADD_SUBR(subr_get_color);

    INTERN(foreground);
    INTERN(background);
    INTERN(underline);
    INTERN(bold);
    INTERN(italic);
    INTERN(inverted);
    INTERN(boxed);
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

    if(fg && bg && bl && hl && ml)
    {
	face = cmd_make_face(VSYM(sym_default_face)->name);
	cmd_set_face_attribute(face, sym_foreground, fg);
	cmd_set_face_attribute(face, sym_background, bg);
	VSYM(sym_default_face)->value = face;
	mouse_cursor_face = face;
	sys_recolor_cursor(mouse_cursor_face);

	face = cmd_make_face(VSYM(sym_block_face)->name);
	cmd_set_face_attribute(face, sym_background, bl);
	VSYM(sym_block_face)->value = face;

	face = cmd_make_face(VSYM(sym_modeline_face)->name);
	cmd_set_face_attribute(face, sym_foreground, fg);
	cmd_set_face_attribute(face, sym_background, ml);
	VSYM(sym_modeline_face)->value = face;

	face = cmd_make_face(VSYM(sym_highlight_face)->name);
	cmd_set_face_attribute(face, sym_background, hl);
	VSYM(sym_highlight_face)->value = face;

	return TRUE;
    }
    else
	return FALSE;
}
