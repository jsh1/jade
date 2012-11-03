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
#include <string.h>
#include <assert.h>


/* faces */

Lisp_Face *allocated_faces;

int face_type;

rep_bool invert_all_faces;

DEFSYM(foreground, "foreground");
DEFSYM(background, "background");
DEFSYM(underline, "underline");
DEFSYM(bold, "bold");
DEFSYM(italic, "italic");
DEFSYM(inverted, "inverted");
DEFSYM(boxed, "boxed");

DEFSYM(default_face, "default-face");
DEFSYM(block_face, "block-face");
DEFSYM(modeline_face, "modeline-face");
DEFSYM(highlight_face, "highlight-face");
DEFSYM(face, "face");
DEFSYM(mouse_face, "mouse-face");

repv mouse_cursor_face;

char *default_fg_color = "black";
char *default_bg_color = "#cdcdc1";
char *default_block_color = "#add8e6";	/* lightblue */
char *default_hl_color = "#eedd82";	/* lightgoldenrod */
char *default_ml_color = "#b0c4de";	/* lightsteelblue */

DEFUN("make-face", Fmake_face, Smake_face, (repv name), rep_Subr1) /*
::doc:make-face::
make-face NAME

Return the face called NAME. If no face with this name exists already, create
a new one, its initial state will be to have absolutely no effect.
::end:: */
{
    Lisp_Face *f;

    rep_DECLARE1(name, rep_STRINGP);

    for(f = allocated_faces; f != 0; f = f->next)
    {
	if(strcmp(rep_STR(f->name), rep_STR(name)) == 0)
	    return rep_VAL(f);
    }

    f = rep_ALLOC_CELL(sizeof(Lisp_Face));
    if(f == 0)
	return rep_mem_error();
    f->next = allocated_faces;
    allocated_faces = f;
    f->name = name;
    f->car = face_type;
    f->foreground = f->background = Qnil;

    return rep_VAL(f);
}

static void
face_prin(repv strm, repv obj)
{
    char buf[128];
#ifdef HAVE_SNPRINTF
    snprintf(buf, sizeof(buf), "#<face %s>", rep_STR(VFACE(obj)->name));
#else
    sprintf(buf, "#<face %s>", rep_STR(VFACE(obj)->name));
#endif
    rep_stream_puts(strm, buf, -1, FALSE);
}

static void
face_sweep(void)
{
    Lisp_Face *f = allocated_faces;
    allocated_faces = 0;
    while(f != 0)
    {
	Lisp_Face *next = f->next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(f)))
	    rep_FREE_CELL(f);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(f));
	    f->next = allocated_faces;
	    allocated_faces = f;
	}
	f = next;
    }
}

static void
face_mark (repv val)
{
    rep_MARKVAL(VFACE(val)->name);
    rep_MARKVAL(VFACE(val)->foreground);
    rep_MARKVAL(VFACE(val)->background);
}

DEFUN("set-face-attribute", Fset_face_attribute, Sset_face_attribute,
      (repv face, repv attr, repv value), rep_Subr3) /*
::doc:set-face-attribute::
set-face-attribute FACE ATTRIBUTE-NAME repv

Set the attribute named ATTRIBUTE-NAME of FACE to repv. ATTRIBUTE-NAME
may be one of these symbols:

  `foreground', `background'
	The colours used for glyphs and blank space respectably. Note
	that repv may be either the color object, or the name of the
	color.

  `underline', 'bold', `italic', `inverted', `boxed'
	Boolean attributes.
::end:: */
{
    WIN *w;
    rep_DECLARE1(face, FACEP);

    if(attr == Qforeground || attr == Qbackground)
    {
	if(rep_STRINGP(value))
	{
	    value = Fget_color(value);
	    if(!value)
		return value;
	}
	if(!rep_NILP(value))
	    rep_DECLARE3(value, COLORP);
	if(attr == Qforeground)
	    VFACE(face)->foreground = value;
	else
	    VFACE(face)->background = value;
    }
    else if(attr == Qunderline)
    {
	VFACE(face)->car &= ~FACEFF_UNDERLINE;
	if(!rep_NILP(value))
	    VFACE(face)->car |= FACEFF_UNDERLINE;
    }
    else if(attr == Qbold)
    {
	VFACE(face)->car &= ~FACEFF_BOLD;
	if(!rep_NILP(value))
	    VFACE(face)->car |= FACEFF_BOLD;
    }
    else if(attr == Qitalic)
    {
	VFACE(face)->car &= ~FACEFF_ITALIC;
	if(!rep_NILP(value))
	    VFACE(face)->car |= FACEFF_ITALIC;
    }
    else if(attr == Qinverted)
    {
	VFACE(face)->car &= ~FACEFF_INVERT;
	if(!rep_NILP(value))
	    VFACE(face)->car |= FACEFF_INVERT;
    }
    else if(attr == Qboxed)
    {
	VFACE(face)->car &= ~FACEFF_BOXED;
	if(!rep_NILP(value))
	    VFACE(face)->car |= FACEFF_BOXED;
    }
    else
	return rep_signal_arg_error(attr, 2);

    for(w = win_chain; w != 0; w = w->next)
    {
	if(w->w_Window)
	    /* TODO: this is highly suboptimal... */
	    w->car |= WINFF_FORCE_REFRESH;
    }
    if(face == mouse_cursor_face)
	sys_recolor_cursor(face);

    return value;
}

DEFUN("face-attribute", Fface_attribute, Sface_attribute,
      (repv face, repv attr), rep_Subr2) /*
::doc:face-attribute::
face-attribute FACE ATTRIBUTE-NAME

Return the current value of the attribute named ATTRIBUTE-NAME in FACE.
See `set-face-attribute' for the list of possible attributes.
::end:: */
{
    rep_DECLARE1(face, FACEP);

    if(attr == Qforeground)
	return VFACE(face)->foreground;
    else if(attr == Qbackground)
	return VFACE(face)->background;
    else if(attr == Qunderline)
	return (VFACE(face)->car & FACEFF_UNDERLINE) ? Qt : Qnil;
    else if(attr == Qbold)
	return (VFACE(face)->car & FACEFF_BOLD) ? Qt : Qnil;
    else if(attr == Qitalic)
	return (VFACE(face)->car & FACEFF_ITALIC) ? Qt : Qnil;
    else if(attr == Qinverted)
	return (VFACE(face)->car & FACEFF_INVERT) ? Qt : Qnil;
    else if(attr == Qboxed)
	return (VFACE(face)->car & FACEFF_BOXED) ? Qt : Qnil;
    else
	return rep_signal_arg_error(attr, 2);
}

DEFUN("mouse-cursor-face", Fmouse_cursor_face, Smouse_cursor_face,
      (repv arg), rep_Subr1) /*
::doc:mouse-cursor-face::
The face used to color the mouse cursor.
::end:: */
{
    if(arg != Qnil)
    {
	mouse_cursor_face = arg;
	sys_recolor_cursor(arg);
    }
    return mouse_cursor_face;
}
    

/* rendering with faces */

static int
get_merged_face(WIN *w, u_long car,
		Lisp_Color *background, Lisp_Color *foreground)
{
    int id, empty = -1;

    /* Find a space in the MergedFaces table, either containing
       an equivalent of this one, or an empty slot. */
    for(id = 0; id <= GA_LastFace; id++)
    {
	if(w->merged_faces[id].valid)
	{
	    Merged_Face *f = &w->merged_faces[id];
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
    w->merged_faces[empty].car = car;
    w->merged_faces[empty].valid = TRUE;
    w->merged_faces[empty].background = background;
    w->merged_faces[empty].foreground = foreground;
    return empty;
}

struct merge_closure
{
    u_long car;
    Lisp_Color *background;
    Lisp_Color *foreground;
};

static void union_face (struct merge_closure *c, Lisp_Face *face)
{
    c->car |= face->car & (FACEFF_MASK & ~FACEFF_INVERT);
    if(face->car & FACEFF_INVERT)
	c->car ^= FACEFF_INVERT;
    if(c->background == 0 && COLORP(face->background))
	c->background = VCOLOR(face->background);
    if(c->foreground == 0 && COLORP(face->foreground))
	c->foreground = VCOLOR(face->foreground);
}

/* Return the id of a face in W->merged_faces that expresses the
   attributes of the positions within E. */
int
merge_faces(VW *vw, Lisp_Extent *e, int in_block, int on_cursor)
{
    WIN *w = vw->window;
    struct merge_closure c;

    Lisp_Extent *x;
    bool pointer_extent = FALSE;

    c.car = invert_all_faces ? FACEFF_INVERT : 0;
    c.background = 0;
    c.foreground = 0;

    if(on_cursor)
    {
	if(WINDOW_HAS_FOCUS(w))
	    c.car |= FACEFF_INVERT;
	else
	    c.car |= FACEFF_BOXED;
    }

    if(in_block)
    {
	repv face = Fsymbol_value (Qblock_face, Qt);
	if(FACEP(face))
	    union_face(&c, VFACE(face));
    }

    /* Work up from E to the root. */
    for(x = e; x != 0; x = x->parent)
    {
	repv face;
	if (!pointer_extent && vw->pointer_extents_count > 0)
	{
	    Lisp_Extent *first = x;
	    int i;
	    while (first->frag_pred != 0)
		first = first->frag_pred;
	    for (i = 0; i < vw->pointer_extents_count; i++)
	    {
		if (first == vw->pointer_extents[i])
		{
		    /* If an inner extent contains the mouse,
		       then all parents of this extent also
		       contain the mouse pointer. */
		    pointer_extent = TRUE;
		    break;
		}
	    }
	}
	if (pointer_extent)
	{
	    face = Fextent_get (rep_VAL(x), Qmouse_face);
	    if (face && FACEP (face))
		union_face (&c, VFACE (face));
	}
	face = Fextent_get(rep_VAL(x), Qface);
	if(face && FACEP(face))
	    union_face(&c, VFACE(face));
    }

    /* Merge in the default-face properties */
    {
	repv face = Fsymbol_value (Qdefault_face, Qt);
	if(face != rep_NULL && FACEP(face))
	    union_face(&c, VFACE(face));
    }

    assert(c.background != 0 && c.foreground != 0);

    return get_merged_face(w, c.car, c.background, c.foreground);
}

int
get_face_id(WIN *w, Lisp_Face *f)
{
    long car = f->car;
    if (invert_all_faces)
	car ^= FACEFF_INVERT;
    assert(COLORP(f->background) && COLORP(f->foreground));
    return get_merged_face(w, car,
			   VCOLOR(f->background), VCOLOR(f->foreground));
}

static void
mark_glyph_buf_faces(WIN *w, glyph_buf *g)
{
    int row, col;
    int id;

    for(id = 0; id <= GA_LastFace; id++)
	w->merged_faces[id].valid = FALSE;

    for(row = 0; row < g->rows; row++)
    {
	glyph_attr *attrs = g->attrs[row];
	for(col = 0; col < g->cols; col++)
	{
	    if(attrs[col] <= GA_LastFace)
		w->merged_faces[attrs[col]].valid = TRUE;
	    else if (attrs[col] != GA_Garbage)
		fprintf (stderr, "warning: invalid glyph attr (%d,%d) = %d\n",
			 row, col, attrs[col]);
	}
    }

    for(id = 0; id <= GA_LastFace; id++)
    {
	if(w->merged_faces[id].valid)
	{
	    rep_MARKVAL(rep_VAL(w->merged_faces[id].background));
	    rep_MARKVAL(rep_VAL(w->merged_faces[id].foreground));
	}
    }
}

void
mark_merged_faces(WIN *w)
{
    mark_glyph_buf_faces(w, w->content);
    mark_glyph_buf_faces(w, w->new_content);
}


/* colors */

Lisp_Color *allocated_colors;

int color_type;

DEFUN("get-color", Fget_color, Sget_color, (repv name), rep_Subr1) /*
::doc:get-color::
get-color NAME

Return the Lisp object defining the color named NAME (a string).
::end:: */
{
    Lisp_Color *c;
    rep_DECLARE1(name, rep_STRINGP);

    for(c = allocated_colors; c != 0; c = c->next)
    {
	if(strcasecmp(rep_STR(c->name), rep_STR(name)) == 0)
	    return rep_VAL(c);
    }

    c = rep_ALLOC_CELL(sizeof(Lisp_Color));
    if(c == 0)
	return rep_mem_error();
    c->car = color_type;
    c->next = allocated_colors;
    allocated_colors = c;
    c->name = name;

    return sys_make_color(c);
}

static void
color_prin(repv strm, repv obj)
{
    char buf[128];
#ifdef HAVE_SNPRINT
    snprintf
#else
    sprintf
#endif
    (buf, "#<color %s>", rep_STR(VCOLOR(obj)->name));
    rep_stream_puts(strm, buf, -1, FALSE);
}

static void
color_sweep(void)
{
    Lisp_Color *c = allocated_colors;
    allocated_colors = 0;
    while(c != 0)
    {
	Lisp_Color *next = c->next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(c)))
	{
	    sys_free_color(c);
	    rep_FREE_CELL(c);
	}
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(c));
	    c->next = allocated_colors;
	    allocated_colors = c;
	}
	c = next;
    }
}

static void
color_mark (repv val)
{
    rep_MARKVAL(VCOLOR(val)->name);
}



bool
faces_init(void)
{
    repv face, fg, bg, bl, hl, ml;

    face_type = rep_register_new_type ("face", 0, face_prin, face_prin,
				       face_sweep, face_mark,
				       0, 0, 0, 0, 0, 0, 0);

    color_type = rep_register_new_type ("color", 0, color_prin, color_prin,
					color_sweep, color_mark,
					0, 0, 0, 0, 0, 0, 0);

    rep_ADD_SUBR(Smake_face);
    rep_ADD_SUBR(Sset_face_attribute);
    rep_ADD_SUBR(Sface_attribute);
    rep_ADD_SUBR(Smouse_cursor_face);
    rep_mark_static(&mouse_cursor_face);
    rep_ADD_SUBR(Sget_color);

    rep_INTERN(foreground);
    rep_INTERN(background);
    rep_INTERN(underline);
    rep_INTERN(bold);
    rep_INTERN(italic);
    rep_INTERN(inverted);
    rep_INTERN(boxed);
    rep_INTERN_SPECIAL(default_face);
    rep_INTERN_SPECIAL(block_face);
    rep_INTERN_SPECIAL(modeline_face);
    rep_INTERN_SPECIAL(highlight_face);
    rep_INTERN(face);
    rep_INTERN(mouse_face);

    fg = Fget_color(rep_string_dup(default_fg_color));
    bg = Fget_color(rep_string_dup(default_bg_color));
    bl = Fget_color(rep_string_dup(default_block_color));
    hl = Fget_color(rep_string_dup(default_hl_color));
    ml = Fget_color(rep_string_dup(default_ml_color));

    if(fg && bg && bl && hl && ml)
    {
	face = Fmake_face(rep_SYM(Qdefault_face)->name);
	Fset_face_attribute(face, Qforeground, fg);
	Fset_face_attribute(face, Qbackground, bg);
	Fset (Qdefault_face, face);
	mouse_cursor_face = face;
	sys_recolor_cursor(mouse_cursor_face);

	face = Fmake_face(rep_SYM(Qblock_face)->name);
	Fset_face_attribute(face, Qbackground, bl);
	Fset (Qblock_face, face);

	face = Fmake_face(rep_SYM(Qmodeline_face)->name);
	Fset_face_attribute(face, Qforeground, fg);
	Fset_face_attribute(face, Qbackground, ml);
	Fset (Qmodeline_face, face);

	face = Fmake_face(rep_SYM(Qhighlight_face)->name);
	Fset_face_attribute(face, Qbackground, hl);
	Fset (Qhighlight_face, face);

	return TRUE;
    }
    else
	return FALSE;
}
