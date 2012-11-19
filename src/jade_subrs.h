/* jade_subrs.h -- Prototypes
   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef JADE_SUBRS_H
#define JADE_SUBRS_H

/* from buffers.c */
extern Lisp_Buffer *first_buffer(void);
extern Lisp_Buffer *swap_buffers(Lisp_View *, Lisp_Buffer *);
extern repv *get_buffer_cursor_ptr(Lisp_Buffer *tx);
extern repv get_buffer_cursor(Lisp_Buffer *);
extern bool auto_save_buffers(bool);
extern void kill_buffer_local_variables(Lisp_Buffer *tx);
extern void buffers_init(void);
extern void buffers_kill(void);
extern int buffer_type;
extern Lisp_Buffer *buffer_chain;
extern repv Qauto_save_function;
extern repv Fmake_buffer_name(repv);
extern repv Fmake_buffer(repv, repv, repv);
extern repv Fget_file_buffer(repv);
extern repv Fget_buffer(repv);
extern repv Fcurrent_buffer(repv);
extern repv Fset_current_buffer(repv, repv);
extern repv Fbuffer_file_name(repv);
extern repv Fset_buffer_file_name(repv, repv);
extern repv Fbuffer_name(repv);
extern repv Fset_buffer_name(repv, repv);
extern repv Fbuffer_changes(repv);
extern repv Fbuffer_modified_p(repv);
extern repv Fset_buffer_modified(repv, repv);
extern repv Fbuffer_length(repv);
extern repv Fline_length(repv, repv);
extern repv Fbufferp(repv);
extern repv Frestrict_buffer(repv start, repv end, repv tx);
extern repv Funrestrict_buffer(repv tx);
extern repv Frestriction_start(repv tx);
extern repv Frestriction_end(repv tx);
extern repv Fbuffer_restricted_p(repv tx);
extern repv var_auto_save_interval(repv);
extern repv var_last_save_changes(repv);
extern repv var_last_user_save_changes(repv);
extern repv var_last_save_time(repv);
extern repv var_tab_size(repv);
extern repv var_truncate_lines(repv);
extern repv var_buffer_status_id(repv);
extern repv Fall_buffers(void);

extern int mark_type;
extern repv Fmake_mark(repv, repv);
extern repv Fset_mark_pos(repv mark, repv pos);
extern repv Fset_mark_file(repv mark, repv file);
extern repv Fmark_pos(repv);
extern repv Fmark_file(repv);
extern repv Fmark_resident_p(repv);
extern repv Fmarkp(repv);

/* from commands.c */
extern repv Qthis_command, Qlast_command, Qprefix_arg, Qcurrent_prefix_arg;
extern void commands_init(void);
extern repv Qprompt_for_function, Qprompt_for_buffer;
extern repv Qprompt_for_char, Qprompt_for_command;
extern repv Qprompt_for_directory, Qprompt_for_file;
extern repv Qprompt_for_number, Qprompt_for_string;
extern repv Qprompt_for_symbol, Qprompt_for_variable;
extern repv Qprompt_for_lisp, Qread_event;
extern repv Qset_auto_mark;
extern repv Qinteractive;
extern repv this_command, last_command;
extern repv Qpre_command_hook, Qpost_command_hook;
extern repv var_this_command(repv val);
extern repv var_last_command(repv val);
extern repv var_prefix_arg(repv val);
extern repv var_current_prefix_arg(repv val);
extern repv Fcall_command(repv cmd, repv arg);
extern repv Fprefix_numeric_argument(repv arg);
extern repv Finteractive(repv spec);
extern repv Fcommandp(repv cmd);

/* from edit.c */
extern bool clear_line_list(Lisp_Buffer *);
extern void kill_line_list(Lisp_Buffer *);
extern LINE *resize_line_list(Lisp_Buffer *, intptr_t, intptr_t);
extern char *alloc_line_buf(Lisp_Buffer *, intptr_t length);
extern void free_line_buf(Lisp_Buffer *tx, char *line);
extern bool insert_gap(Lisp_Buffer *, intptr_t, intptr_t, intptr_t);
extern repv insert_bytes(Lisp_Buffer *, const char *, size_t, repv);
extern repv insert_string(Lisp_Buffer *, const char *, size_t, repv);
extern bool delete_chars(Lisp_Buffer *, intptr_t, intptr_t, intptr_t);
extern repv delete_section(Lisp_Buffer *, repv, repv);
extern bool pad_pos(Lisp_Buffer *, repv);
extern bool pad_cursor(Lisp_View *);
extern void order_pos(repv *, repv *);
extern bool check_section(Lisp_Buffer *, repv *, repv *);
extern repv check_pos(Lisp_Buffer *, repv);
extern bool check_line(Lisp_Buffer *, repv);
extern bool check_row(Lisp_Buffer *tx, intptr_t line);
extern size_t section_length(Lisp_Buffer *, repv, repv);
extern void copy_section(Lisp_Buffer *, repv, repv, char *);
extern void order_block(Lisp_View *);
extern bool read_only_pos(Lisp_Buffer *, repv);
extern bool read_only_section(Lisp_Buffer *, repv, repv);
extern repv make_pos(intptr_t, intptr_t);
extern void edit_init(void);

/* from editcommands.c */
extern repv Qblock_status_hook;
extern repv Qinhibit_read_only, Qread_only;
extern repv cmd_pos(repv, repv);
extern repv Finsert(repv string, repv pos, repv buff);
extern repv Fdelete_area(repv start, repv end, repv buff);
extern repv Fcopy_area(repv start, repv end, repv buff);
extern repv Fcut_area(repv start, repv end, repv buff);
extern repv Fblock_toggle(void);
extern repv Fblock_start(repv pos);
extern repv Fblock_end(repv pos);
extern repv Fblock_kill(void);
extern repv Fblockp(void);
extern repv Ftranslate_area(repv start, repv end, repv table, repv tx);
extern repv Fget_char(repv pos, repv tx);
extern repv Fset_char(repv ch, repv pos, repv tx);
extern repv Fposp(repv arg);
extern repv Fcursor_pos(void);
extern repv Fempty_line_p(repv pos, repv tx);
extern repv Findent_pos(repv pos, repv tx);
extern repv Fset_indent_pos(repv indpos, repv tx, repv spaces_p);
extern repv Findent_to(repv col, repv spaces_p);
extern repv Fclear_buffer(repv tx);
extern repv Fpos_to_offset(repv pos, repv tx);
extern repv Foffset_to_pos(repv voffset, repv tx);
extern repv Fcall_process_area(repv arg_list);

/* from extent.c */
extern Lisp_Extent *find_extent_forwards(Lisp_Extent *root, Pos *pos);
extern Lisp_Extent *find_extent(Lisp_Extent *root, Pos *pos);
extern void map_section_extents(void (*)(Lisp_Extent *, void *),
				Lisp_Extent *, Pos *, Pos *, void *);
extern void make_global_extent(Lisp_Buffer *tx);
extern void reset_global_extent(Lisp_Buffer *tx);
extern bool buffer_set_if_bound(repv symbol, repv value);
extern void adjust_extents_add_cols(Lisp_Extent *, intptr_t,
				    intptr_t, intptr_t);
extern void adjust_extents_sub_cols(Lisp_Extent *, intptr_t,
				    intptr_t, intptr_t);
extern void adjust_extents_add_rows(Lisp_Extent *, intptr_t,
				    intptr_t);
extern void adjust_extents_sub_rows(Lisp_Extent *, intptr_t,
				    intptr_t);
extern void adjust_extents_split_row(Lisp_Extent *, intptr_t,
				     intptr_t);
extern void adjust_extents_join_rows(Lisp_Extent *, intptr_t,
				     intptr_t);
extern void extent_init(void);
extern repv Qfront_sticky, Qrear_sticky;
extern repv Qlocal_variables, Qcatch_variables;
extern int extent_type;
extern repv Fmake_extent(repv, repv, repv);
extern repv Fdelete_extent(repv);
extern repv Fdelete_all_extents(repv);
extern repv Fmove_extent(repv, repv, repv);
extern repv Fget_extent(repv, repv);
extern repv Fmap_extents(repv, repv, repv);
extern repv Fextent_start(repv);
extern repv Fextent_end(repv);
extern repv Fextent_parent(repv);
extern repv Fextent_root(repv);
extern repv Fextent_plist(repv);
extern repv Fset_extent_plist(repv, repv);
extern repv Fextent_get(repv, repv);
extern repv Fextent_put(repv, repv, repv);
extern repv Fbuffer_get(repv, repv, repv);
extern repv Fbuffer_symbol_value(repv, repv, repv, repv);
extern repv Fextent_set(repv extent, repv symbol, repv val);
extern repv Fmake_variable_buffer_local(repv);
extern repv Fmake_variable_buffer_local(repv);
extern repv Fbuffer_variables(repv);
extern repv Fkill_all_local_variables(repv);
extern repv Fkill_local_variable(repv, repv);
extern void start_visible_extent (Lisp_View *vw, Lisp_Extent *e, intptr_t
				  start_col, intptr_t start_row);
extern void end_visible_extent (Lisp_View *vw, Lisp_Extent *e,
				intptr_t end_col, intptr_t end_row);
extern void free_visible_extents (Lisp_Window *w);
extern void map_visible_extents (Lisp_Window *w, intptr_t col,
				 intptr_t row,
				 void (*fun)(struct visible_extent *x));
extern void mark_visible_extents (Lisp_Window *w);
extern bool update_pointer_extent (Lisp_Window *w, intptr_t mouse_col,
				   intptr_t mouse_row);

/* from faces.c */
extern bool invert_all_faces;
extern int merge_faces(Lisp_View *vw, Lisp_Extent *e, int in_active, int on_cursor);
extern int get_face_id(Lisp_Window *w, Lisp_Face *f);
extern void mark_merged_faces(Lisp_Window *w);
extern bool faces_init(void);
extern Lisp_Face *allocated_faces;
extern int face_type;
extern repv Qforeground, Qbackground, Qunderline, Qbold, Qitalic;
extern repv Qinverted, Qboxed;
extern repv Qdefault_face, Qblock_face, Qmodeline_face;
extern repv Qhighlight_face, Qface;
extern repv mouse_cursor_face;
extern char *default_fg_color, *default_bg_color;
extern char *default_block_color, *default_hl_color, *default_ml_color;
extern repv Fmake_face(repv);
extern repv Fset_face_attribute(repv, repv, repv);
extern repv Fface_attribute(repv, repv);
extern repv var_mouse_cursor_face(repv);
extern Lisp_Color *allocated_colors;
extern int color_type;
extern repv Fget_color(repv);

/* from files.c */
extern repv Fwrite_buffer_contents(repv, repv, repv);
extern repv Fread_file_contents(repv);
extern repv Finsert_file_contents(repv);
extern void files_init(void);

/* from find.c */
extern bool buffer_strpbrk(Lisp_Buffer *tx, Pos *pos, const char *chars);
extern bool buffer_reverse_strpbrk(Lisp_Buffer *tx, Pos *pos,
				   const char *chars);
extern bool buffer_strchr(Lisp_Buffer *tx, Pos *pos, char c);
extern bool buffer_reverse_strchr(Lisp_Buffer *tx, Pos *pos, char c);
extern bool buffer_compare_n(Lisp_Buffer *tx, Pos *pos, const char *str,
			     int n, void *cmpfn);
extern bool forward_char(long count, Lisp_Buffer *tx, Pos *pos);
extern bool backward_char(long count, Lisp_Buffer *tx, Pos *pos);
extern void find_init(void);
extern repv Fre_search_forward(repv re, repv pos, repv tx, repv nocase_p);
extern repv Fre_search_backward(repv re, repv pos, repv tx, repv nocase_p);
extern repv Fsearch_forward(repv str, repv pos, repv tx, repv nocasep);
extern repv Fsearch_backward(repv str, repv pos, repv tx, repv nocasep);
extern repv Fchar_search_forward(repv ch, repv pos, repv tx);
extern repv Fchar_search_backward(repv ch, repv pos, repv tx);
extern repv Flooking_at(repv re, repv pos, repv tx, repv nocase_p);
extern repv Fbuffer_compare_string(repv, repv, repv, repv);

/* from glyphs.c */
extern void make_window_glyphs(glyph_buf *g, Lisp_Window *w);
extern void make_message_glyphs(glyph_buf *g, Lisp_Window *w);
extern bool skip_glyph_rows_forwards(Lisp_View *, intptr_t,
				     intptr_t, intptr_t,
				     intptr_t *, intptr_t *);
extern bool skip_glyph_rows_backwards(Lisp_View *, intptr_t, intptr_t,
				      intptr_t, intptr_t *,
				      intptr_t *);
extern void recenter_cursor(Lisp_View *vw);
extern intptr_t glyph_col(Lisp_Buffer *, intptr_t, intptr_t);
extern intptr_t char_col(Lisp_Buffer *, intptr_t, intptr_t);
extern intptr_t get_cursor_column(Lisp_View *);
extern void set_cursor_vertically(Lisp_View *vw, intptr_t row);
extern void glyphs_init(void);
extern void glyphs_kill(void);
extern repv Qglyph_table;
extern int glyph_table_type;
extern repv Fglyph_table_p(repv arg);
extern repv Fchar_to_glyph_pos(repv pos, repv tx);
extern repv Fglyph_to_char_pos(repv pos, repv tx);
extern repv Fdisplay_to_char_pos(repv pos, repv vw);
extern repv Fchar_to_display_pos(repv pos, repv vw);
extern repv Fdefault_glyph_table(void);
extern repv Fmake_glyph_table(repv src);
extern repv Fset_glyph(repv gt, repv ch, repv glyph);
extern repv Fget_glyph(repv gt, repv ch);

/* from housekeeping.c */
extern void adjust_marks_add_x(Lisp_Buffer *, intptr_t, intptr_t,
			       intptr_t);
extern void adjust_marks_sub_x(Lisp_Buffer *, intptr_t, intptr_t,
			       intptr_t);
extern void adjust_marks_add_y(Lisp_Buffer *, intptr_t, intptr_t);
extern void adjust_marks_sub_y(Lisp_Buffer *, intptr_t, intptr_t);
extern void adjust_marks_split_y(Lisp_Buffer *, intptr_t, intptr_t);
extern void adjust_marks_join_y(Lisp_Buffer *, intptr_t, intptr_t);
extern void reset_all_views(Lisp_Buffer *);

/* from keys.c */
extern repv eval_input_event(void *, unsigned long, unsigned long);
extern bool lookup_event(unsigned long *, unsigned long *, char *);
extern bool lookup_event_name(char *, unsigned long, unsigned long);
extern bool print_event_prefix(void);
extern void keys_init(void);
extern unsigned long current_event[2], last_event[2];
extern repv Qglobal_keymap, Qlocal_keymap, Qunbound_key_hook;
extern repv Qesc_means_meta, Qkeymap, Qoverriding_local_keymap;
extern repv Qminor_mode_keymap_alist, Qautoload_keymap;
extern repv Qnext_keymap_path;
extern repv Qidle_hook;
extern repv Fmake_keymap(void);
extern repv Fmake_sparse_keymap(repv base);
extern repv Fbind_keys(repv args);
extern repv Funbind_keys(repv args);
extern repv Fnext_keymap_path(repv path);
extern repv Fcurrent_event_string(void);
extern repv Fcurrent_event(void);
extern repv Flast_event(void);
extern repv Fevent_name(repv ev);
extern repv Flookup_event(repv name);
extern repv Flookup_event_binding(repv ev);
extern repv Fsearch_keymap(repv ev, repv km);
extern repv Fkeymapp(repv arg);
extern repv Feventp(repv arg);

/* from main.c */
extern bool batch_mode_p (void);
extern int main(int, char **);

/* from misc.c */
extern void misc_init(void);
extern repv Qwindow_system, Qjade_build_id_string;
extern repv Qjade_major_version, Qjade_minor_version;
extern repv Qinvalid_area, Qwindow_error, Qinvalid_pos;
extern repv Qbuffer_read_only, Qbad_event_desc;

/* from movement.c */
extern void movement_init(void);
extern repv Fgoto(repv pos);
extern repv Fgoto_glyph(repv pos);
extern repv Fcenter_display(repv vw, repv arg);
extern repv Fnext_screen(repv number);
extern repv Fprev_screen(repv number);
extern repv Fend_of_buffer(repv tx, repv irp);
extern repv Fstart_of_buffer(repv tx, repv irp);
extern repv Fend_of_line(repv pos, repv tx);
extern repv Fstart_of_line(repv pos);
extern repv Fforward_line(repv lines, repv pos);
extern repv Fforward_char(repv count, repv pos, repv tx);
extern repv Fforward_tab(repv num, repv pos, repv size);
extern repv Ffind_matching_bracket(repv pos, repv tx, repv esc);
extern repv Fraw_mouse_pos(void);

/* from redisplay.c */
extern glyph_buf *alloc_glyph_buf(intptr_t cols, intptr_t rows);
extern void free_glyph_buf(glyph_buf *gb);
extern void copy_glyph_buf(glyph_buf *dst, glyph_buf *src);
extern void garbage_glyphs(Lisp_Window *w, intptr_t x, intptr_t y, intptr_t width, intptr_t height);
extern void redisplay_message(Lisp_Window *w);
extern void redisplay_init(void);
extern int redisplay_lock;
extern repv Fredisplay_window(repv win, repv arg);
extern repv Fredisplay(repv arg);
extern repv var_redisplay_max_d(repv val);
extern void redisplay_set_no_copy (void);

/* from regjade.c */
extern int regexec_buffer(rep_regexp *prog, Lisp_Buffer *tx, repv start, int flags);
extern int regexec_reverse_buffer(rep_regexp *prog, Lisp_Buffer *tx, repv start, int flags);
extern int regmatch_buffer(rep_regexp *prog, Lisp_Buffer *tx, repv start, int flags);

/* from regsub.c */
extern void jade_regsub(int lasttype, rep_regsubs *matches,
			char *source, char *dest, void *data);
extern size_t jade_regsublen(int lasttype, rep_regsubs *matches,
			     char *source, void *data);

/* from undo.c */
extern void undo_record_unmodified(Lisp_Buffer *tx);
extern void undo_record_deletion(Lisp_Buffer *, repv, repv);
extern repv undo_push_deletion(Lisp_Buffer *, repv, repv);
extern void undo_record_insertion(Lisp_Buffer *, repv, repv);
extern void undo_record_modification(Lisp_Buffer *, repv, repv);
extern void undo_end_of_command(void);
extern void undo_trim(void);
extern void undo_init(void);
extern repv Qundo;
extern repv Fundo(repv tx, repv arg);
extern repv var_max_undo_size(repv val);
extern repv var_buffer_record_undo(repv val);

/* from views.c */
extern void kill_all_views(Lisp_Window *w);
extern void update_views_dimensions(Lisp_Window *w);
extern void update_status_buffer(Lisp_View *vw, char *status_buf, intptr_t buflen);
extern void views_init(void);
extern void views_kill(void);
extern Lisp_View *make_view(Lisp_View *, Lisp_Window *, Lisp_Buffer *, intptr_t, bool);
extern repv Qsplit_view_hook, Qdelete_view_hook;
extern int view_type;
extern Lisp_View *view_chain, *curr_vw;
extern Lisp_Buffer *mb_unused_buffer;
extern repv Fsplit_view(repv sib, repv lines);
extern repv Fdelete_view(repv view);
extern repv var_y_scroll_step_ratio(repv val);
extern repv var_x_scroll_step_ratio(repv val);
extern repv Frect_blocks_p(repv vw);
extern repv Fset_rect_blocks(repv vw, repv stat);
extern repv Fcurrent_view(repv win);
extern repv Fset_current_view(repv vw, repv activ);
extern repv var_buffer_list(repv val);
extern repv Fget_buffer_view(repv buffer, repv all_windows);
extern repv Fnext_view(repv win, repv allp);
extern repv Fprevious_view(repv win, repv allp);
extern repv Fview_origin(repv vw);
extern repv Fview_dimensions(repv vw);
extern repv Fview_position(repv vw);
extern repv Fset_view_dimensions(repv vw, repv cols, repv rows);
extern repv Ffind_view_by_pos(repv pos, repv win);
extern repv Ftranslate_pos_to_view(repv pos, repv vw);
extern repv Fminibuffer_view_p(repv vw);
extern repv Fminibuffer_view(repv win);
extern repv Fminibuffer_active_p(repv win);
extern repv Fviewp(repv);

/* from windows.c */
extern repv Qdimensions, Qposition, Qbuffer, Qfont;
extern void update_window_dimensions(Lisp_Window *w);
extern void messagen(char *, size_t);
extern void messagef(char *, va_list);
extern void reset_message(Lisp_Window *);
extern bool remove_all_messages(bool from_idle_p);
extern void windows_init(void);
extern void windows_kill(void);
extern repv Qmake_window_hook, Qdelete_window_hook;
extern int window_type;
extern Lisp_Window *win_chain;
extern Lisp_Window *curr_win;
extern repv def_font_str;
extern void set_default_geometry (int x, int y, int w, int h);
extern repv Fmake_window(repv attr);
extern repv Fdelete_window(repv win);
extern repv Fsleep_window(repv win);
extern repv Funsleep_window(repv win);
extern repv Fnext_window(repv win, repv activ);
extern repv Fmessage(repv string, repv now);
extern repv Ffont_name(repv win);
extern repv Fwindow_asleep_p(void);
extern repv Fposition_window(repv left, repv top, repv width, repv height);
extern repv Fcurrent_window(void);
extern repv Fset_current_window(repv win, repv activ);
extern repv Fwindow_id(repv win);
extern repv Ffont_dimensions(repv win);
extern repv Fwindow_dimensions(repv win);
extern repv Fwindow_list(void);
extern repv Fwindow_view_list(repv win);
extern repv Fwindow_view_count(repv win);
extern repv Fwindow_first_view(repv win);
extern repv Fwindowp(repv);
extern repv Fset_font(repv fontname, repv win);

#if defined (HAVE_GTK)

/* from gtk_keys.c */
extern void translate_event(unsigned long *, unsigned long *, GdkEvent *);
extern size_t sys_cook_key(void *, char *, size_t);
extern bool sys_lookup_mod(const char *name, unsigned long *mods);
extern bool sys_lookup_code(const char *name, unsigned long *code, unsigned long *mods);
extern char *sys_lookup_mod_name(char *buf, unsigned long mod);
extern bool sys_lookup_code_name(char *buf, unsigned long code, unsigned long type);
extern unsigned long gtk_find_meta(void);
extern unsigned long esc_code, esc_mods;

/* from gtk_main.c */
extern void sys_beep(Lisp_Window *w);
extern repv (*gtk_jade_wrap_gtkobj)(GtkObject *object);
extern GtkObject *(*gtk_jade_get_gtkobj)(repv obj);
extern void (*gtk_jade_callback_postfix)(void);
extern repv sys_make_color(Lisp_Color *c);
extern void sys_free_color(Lisp_Color *c);
extern void sys_usage(void);
extern bool sys_init(char *);
extern void sys_kill(void);
extern unsigned long gtk_meta_mod;
extern repv sys_get_mouse_pos(Lisp_Window *);

/* from gtk_select.c */
extern repv Fgtk_jade_set_selection(repv sel, repv start,
				    repv end, repv buffer);
extern repv Fgtk_jade_selection_active_p(repv sel);
extern repv Fgtk_jade_own_selection_p(repv sel);
extern repv Fgtk_jade_get_selection(repv sel);
extern repv Fgtk_jade_lose_selection(repv sel);
extern void gtk_jade_add_selection_targets (GtkJade *jade);
extern void gtk_jade_selection_get(GtkWidget *widget,
				   GtkSelectionData *sel_data,
				   guint info, guint time);
extern gint gtk_jade_selection_clear(GtkWidget *widget,
				     GdkEventSelection *event);
extern void gtk_jade_window_lose_selections(Lisp_Window *w);
extern void gtk_misc_init(void);

/* from gtk_jade.c */
extern unsigned long gtk_jade_last_event_time;
extern GtkWidget *gtk_jade_new (Lisp_Window *win, int width, int height);
extern guint gtk_jade_get_type (void);
extern bool gtk_jade_set_font (GtkJade *jade);
extern void gtk_jade_get_size (GtkJade *jade, gint *widthp, gint *heightp);
extern void sys_draw_glyphs(Lisp_Window *, int, int, uint8_t, char *, int, bool);
extern void sys_recolor_cursor(repv face);
extern void sys_update_dimensions(Lisp_Window *);
extern GtkJade *sys_new_window(Lisp_Window *, Lisp_Window *, int *);
extern void sys_kill_window(Lisp_Window *);
extern bool sys_set_font(Lisp_Window *);
extern void sys_unset_font(Lisp_Window *);
extern bool sys_sleep_win(Lisp_Window *);
extern bool sys_unsleep_win(Lisp_Window *);
extern void sys_activate_win(Lisp_Window *);
extern void sys_set_win_name(Lisp_Window *win, char *name);
extern void sys_set_win_pos(Lisp_Window *, long, long, long, long);
extern bool sys_deleting_window_would_exit (Lisp_Window *w);
extern void sys_windows_init(void);
extern void gtk_jade_foreach (GtkContainer *root,
			      GtkCallback fun, gpointer data);
extern repv Fgtk_jade_new (repv, repv);
extern repv Fgtk_jade_window (repv);
extern repv Fgtk_jade_window_widget (repv);
extern repv Fflush_output(void);

#elif defined (HAVE_MAC)

/* from mac_main.m */
extern unsigned long mac_meta_mod;
extern bool mac_app_is_active;
extern void sys_beep(Lisp_Window *w);
extern bool sys_init(char *);
extern void sys_kill(void);
extern void sys_usage(void);
extern repv sys_make_color(Lisp_Color *c);
extern void sys_free_color(Lisp_Color *c);

/* from mac_windows.m */
extern void sys_begin_redisplay (Lisp_Window *);
extern void sys_end_redisplay (Lisp_Window *);
extern void sys_draw_glyphs (Lisp_Window *, int, int, uint8_t, char *, int, bool);
extern void sys_copy_glyphs (Lisp_Window *, int, int, int, int, int, int);
extern void sys_recolor_cursor(repv face);
extern void sys_update_dimensions(Lisp_Window *);
extern void *sys_new_window(Lisp_Window *, Lisp_Window *, int *);
extern void sys_kill_window(Lisp_Window *);
extern bool sys_sleep_win(Lisp_Window *);
extern bool sys_unsleep_win(Lisp_Window *);
extern void sys_activate_win(Lisp_Window *);
extern void sys_set_win_name(Lisp_Window *win, char *name);
extern void sys_set_win_pos(Lisp_Window *, long, long, long, long);
extern bool sys_deleting_window_would_exit (Lisp_Window *w);
extern bool sys_window_has_focus (Lisp_Window *);
extern bool sys_window_realized (Lisp_Window *);
extern repv sys_get_mouse_pos(Lisp_Window *);
extern repv Fflush_output(void);
extern void sys_windows_init(void);

/* from mac_keys.m */
extern unsigned long esc_code, esc_mods;
extern void sys_translate_event(unsigned long *, unsigned long *, void *);
extern size_t sys_cook_key(void *, char *, size_t);
extern bool sys_lookup_mod(const char *, unsigned long *);
extern bool sys_lookup_code(const char *, unsigned long *, unsigned long *);
extern char *sys_lookup_mod_name(char *, unsigned long);
extern bool sys_lookup_code_name(char *, unsigned long, unsigned long);
extern unsigned long mac_find_meta(void);
extern bool sys_set_font(Lisp_Window *);
extern void sys_unset_font(Lisp_Window *);

/* from mac_runloop.m */
extern bool mac_needs_redisplay;
extern bool mac_defer_event (void *view, void *ns_event);
extern void mac_runloop_init (void);

#elif defined (HAVE_X11)

/* from x11_keys.c */
extern void translate_event(unsigned long *, unsigned long *, XEvent *,
			    struct x11_display *);
extern size_t sys_cook_key(void *, char *, size_t);
extern bool sys_lookup_mod(const char *name, unsigned long *mods);
extern bool sys_lookup_code(const char *name, unsigned long *code, unsigned long *mods);
extern char *sys_lookup_mod_name(char *buf, unsigned long mod);
extern bool sys_lookup_code_name(char *buf, unsigned long code, unsigned long type);
extern unsigned long x11_find_meta(struct x11_display *xd);
extern unsigned long esc_code, esc_mods;

/* from x11_main.c */
extern int x11_cursor_shape;
extern struct x11_display *x11_get_display(char *display_name);
extern struct x11_display *x11_open_display(char *display_name);
extern void x11_close_display(struct x11_display *xdisplay);
extern void x11_close_all_displays(void);
extern struct x11_color *x11_make_color_dpy(Lisp_Color *c,
					    struct x11_display *);
extern repv sys_make_color(Lisp_Color *c);
extern struct x11_color *x11_get_color_dpy(Lisp_Color *c,
					   struct x11_display *);
extern void sys_free_color(Lisp_Color *c);
extern void x11_free_dpy_colors(struct x11_display *dpy);
extern void sys_recolor_cursor(repv face);
extern void x11_handle_async_input(void);
extern void sys_usage(void);
extern bool sys_init(char *);
extern void sys_kill(void);
extern Lisp_Window *x11_current_event_win;
extern long x11_current_mouse_x, x11_current_mouse_y;
extern Time x11_last_event_time;
extern struct x11_display *x11_display_list;
extern char **x11_argv;
extern int x11_argc;
extern bool x11_opt_reverse_video;

/* from x11_misc.c */
extern void sys_beep(Lisp_Window *w);
extern void x11_convert_selection(XSelectionRequestEvent *ev);
extern void x11_lose_selection(XSelectionClearEvent *ev);
extern void x11_window_lose_selections(Lisp_Window *w);
extern void x11_misc_init(void);
extern repv Qxa_primary, Qxa_secondary;
extern repv Fx11_set_selection(repv sel, repv start, repv end, repv buffer);
extern repv Fx11_selection_active_p(repv sel);
extern repv Fx11_own_selection_p(repv sel);
extern repv Fx11_get_selection(repv sel);
extern repv Fx11_lose_selection(repv sel);

/* from x11_windows.c */
extern bool sys_sleep_win(Lisp_Window *);
extern bool sys_unsleep_win(Lisp_Window *);
extern void sys_update_dimensions(Lisp_Window *);
extern void x11_update_dimensions(Lisp_Window *, int, int);
extern Window sys_new_window(Lisp_Window *, Lisp_Window *, int *);
extern void sys_kill_window(Lisp_Window *);
extern void sys_activate_win(Lisp_Window *);
extern void sys_set_win_name(Lisp_Window *win, char *name);
extern void sys_set_win_pos(Lisp_Window *, long, long, long, long);
extern Lisp_Window *x11_find_window(Window);
extern void sys_draw_glyphs(Lisp_Window *, int, int, uint8_t, char *, int, bool);
extern bool sys_set_font(Lisp_Window *);
extern void sys_unset_font(Lisp_Window *);
extern repv sys_get_mouse_pos(Lisp_Window *);
extern bool sys_deleting_window_would_exit (Lisp_Window *w);
extern void sys_windows_init(void);
extern repv Fflush_output(void);
extern repv Fmake_window_on_display(repv display);

#endif /* window system */

#endif /* JADE_SUBRS */
