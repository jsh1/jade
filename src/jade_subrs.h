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
extern TX *first_buffer(void);
extern TX *swap_buffers(VW *, TX *);
extern repv *get_tx_cursor_ptr(TX *tx);
extern repv get_tx_cursor(TX *);
extern int auto_save_buffers(bool);
extern void tx_kill_local_variables(TX *tx);
extern void buffers_init(void);
extern void buffers_kill(void);
extern int buffer_type;
extern TX *buffer_chain;
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
extern bool clear_line_list(TX *);
extern void kill_line_list(TX *);
extern LINE *resize_line_list(TX *, long, long);
extern u_char *alloc_line_buf(TX *, long length);
extern void free_line_buf(TX *tx, u_char *line);
extern bool insert_gap(TX *, long, long, long);
extern repv insert_bytes(TX *, const u_char *, long, repv);
extern repv insert_string(TX *, const u_char *, long, repv);
extern bool delete_chars(TX *, long, long, long);
extern repv delete_section(TX *, repv, repv);
extern bool pad_pos(TX *, repv);
extern bool pad_cursor(VW *);
extern void order_pos(repv *, repv *);
extern bool check_section(TX *, repv *, repv *);
extern repv check_pos(TX *, repv);
extern bool check_line(TX *, repv);
extern bool check_row(TX *tx, long line);
extern long section_length(TX *, repv, repv);
extern void copy_section(TX *, repv, repv, u_char *);
extern void order_block(VW *);
extern bool read_only_pos(TX *, repv);
extern bool read_only_section(TX *, repv, repv);
extern repv make_pos(long, long);
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
extern void map_section_extents(void (*)(Lisp_Extent *, void *), Lisp_Extent *, Pos *, Pos *, void *);
extern void make_global_extent(TX *tx);
extern void reset_global_extent(TX *tx);
extern bool buffer_set_if_bound(repv symbol, repv value);
extern void adjust_extents_add_cols(Lisp_Extent *, long, long, long);
extern void adjust_extents_sub_cols(Lisp_Extent *, long, long, long);
extern void adjust_extents_add_rows(Lisp_Extent *, long, long);
extern void adjust_extents_sub_rows(Lisp_Extent *, long, long);
extern void adjust_extents_split_row(Lisp_Extent *, long, long);
extern void adjust_extents_join_rows(Lisp_Extent *, long, long);
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
extern void start_visible_extent (VW *vw, Lisp_Extent *e,
				  long start_col, long start_row);
extern void end_visible_extent (VW *vw, Lisp_Extent *e,
				long end_col, long end_row);
extern void free_visible_extents (WIN *w);
extern void map_visible_extents (WIN *w, long col, long row,
				 void (*fun)(struct visible_extent *x));
extern void mark_visible_extents (WIN *w);
extern bool update_mouse_extent (WIN *w, long mouse_col, long mouse_row);

/* from faces.c */
extern int invert_all_faces;
extern int merge_faces(VW *vw, Lisp_Extent *e, int in_active, int on_cursor);
extern int get_face_id(WIN *w, Lisp_Face *f);
extern void mark_merged_faces(WIN *w);
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
extern int buffer_strpbrk(TX *tx, Pos *pos, const char *chars);
extern int buffer_reverse_strpbrk(TX *tx, Pos *pos, const char *chars);
extern int buffer_strchr(TX *tx, Pos *pos, char c);
extern int buffer_reverse_strchr(TX *tx, Pos *pos, char c);
extern int buffer_compare_n(TX *tx, Pos *pos, const char *str,
			    int n, void *cmpfn);
extern int forward_char(long count, TX *tx, Pos *pos);
extern int backward_char(long count, TX *tx, Pos *pos);
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
extern void make_window_glyphs(glyph_buf *g, WIN *w);
extern void make_message_glyphs(glyph_buf *g, WIN *w);
extern bool skip_glyph_rows_forwards(VW *, long, long, long, long *, long *);
extern bool skip_glyph_rows_backwards(VW *, long, long, long, long *, long *);
extern void recenter_cursor(VW *vw);
extern long glyph_col(TX *, long, long);
extern long char_col(TX *, long, long);
extern long get_cursor_column(VW *);
extern void set_cursor_vertically(VW *vw, long row);
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
extern void adjust_marks_add_x(TX *, long, long, long);
extern void adjust_marks_sub_x(TX *, long, long, long);
extern void adjust_marks_add_y(TX *, long, long);
extern void adjust_marks_sub_y(TX *, long, long);
extern void adjust_marks_split_y(TX *, long, long);
extern void adjust_marks_join_y(TX *, long, long);
extern void reset_all_views(TX *);

/* from keys.c */
extern repv eval_input_event(void *, u_long, u_long);
extern bool lookup_event(u_long *, u_long *, u_char *);
extern bool lookup_event_name(u_char *, u_long, u_long);
extern bool print_event_prefix(void);
extern void keys_init(void);
extern u_long current_event[2], last_event[2];
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
extern glyph_buf *alloc_glyph_buf(int cols, int rows);
extern void free_glyph_buf(glyph_buf *gb);
extern void copy_glyph_buf(glyph_buf *dst, glyph_buf *src);
extern void garbage_glyphs(WIN *w, int x, int y, int width, int height);
extern void redisplay_message(WIN *w);
extern void redisplay_init(void);
extern int redisplay_lock;
extern repv Fredisplay_window(repv win, repv arg);
extern repv Fredisplay(repv arg);
extern repv var_redisplay_max_d(repv val);

/* from regjade.c */
extern int regexec_tx(rep_regexp *prog, TX *tx, repv start, int flags);
extern int regexec_reverse_tx(rep_regexp *prog, TX *tx, repv start, int flags);
extern int regmatch_tx(rep_regexp *prog, TX *tx, repv start, int flags);

/* from regsub.c */
extern void jade_regsub(int lasttype, rep_regsubs *matches,
			u_char *source, u_char *dest, void *data);
extern int jade_regsublen(int lasttype, rep_regsubs *matches,
			  u_char *source, void *data);

/* from undo.c */
extern void undo_record_unmodified(TX *tx);
extern void undo_record_deletion(TX *, repv, repv);
extern repv undo_push_deletion(TX *, repv, repv);
extern void undo_record_insertion(TX *, repv, repv);
extern void undo_record_modification(TX *, repv, repv);
extern void undo_end_of_command(void);
extern void undo_trim(void);
extern void undo_init(void);
extern repv Qundo;
extern repv Fundo(repv tx, repv arg);
extern repv var_max_undo_size(repv val);
extern repv var_buffer_record_undo(repv val);

/* from views.c */
extern void kill_all_views(WIN *w);
extern void update_views_dimensions(WIN *w);
extern void update_status_buffer(VW *vw, char *status_buf, long buflen);
extern void views_init(void);
extern void views_kill(void);
extern VW *make_view(VW *, WIN *, TX *, long, bool);
extern repv Qsplit_view_hook, Qdelete_view_hook;
extern int view_type;
extern VW *view_chain, *curr_vw;
extern TX *mb_unused_buffer;
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
extern void update_window_dimensions(WIN *w);
extern void messagen(u_char *, int);
extern void messagef(u_char *, va_list);
extern void reset_message(WIN *);
extern bool remove_all_messages(bool from_idle_p);
extern void windows_init(void);
extern void windows_kill(void);
extern repv Qmake_window_hook, Qdelete_window_hook;
extern int window_type;
extern WIN *win_chain;
extern WIN *curr_win;
extern repv def_font_str;
extern void set_default_geometry (short x, short y, short w, short h);
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
extern void translate_event(u_long *, u_long *, GdkEvent *);
extern int cook_key(void *, u_char *, int);
extern bool sys_lookup_mod(const char *name, u_long *mods);
extern bool sys_lookup_code(const char *name, u_long *code, u_long *mods);
extern char *sys_lookup_mod_name(char *buf, u_long mod);
extern bool sys_lookup_code_name(char *buf, u_long code, u_long type);
extern u_long gtk_find_meta(void);
extern u_long esc_code, esc_mods;

/* from gtk_main.c */
extern void sys_beep(WIN *w);
extern repv (*gtk_jade_wrap_gtkobj)(GtkObject *object);
extern GtkObject *(*gtk_jade_get_gtkobj)(repv obj);
extern void (*gtk_jade_callback_postfix)(void);
extern repv sys_make_color(Lisp_Color *c);
extern void sys_free_color(Lisp_Color *c);
extern void sys_usage(void);
extern bool sys_init(char *);
extern void sys_kill(void);
extern u_long gtk_meta_mod;
extern repv sys_get_mouse_pos(WIN *);

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
extern void gtk_jade_window_lose_selections(WIN *w);
extern void gtk_misc_init(void);

/* from gtk_jade.c */
extern u_long gtk_jade_last_event_time;
extern GtkWidget *gtk_jade_new (WIN *win, int width, int height);
extern guint gtk_jade_get_type (void);
extern int gtk_jade_set_font (GtkJade *jade);
extern void gtk_jade_get_size (GtkJade *jade, gint *widthp, gint *heightp);
extern void sys_draw_glyphs(WIN *, int, int, glyph_attr, char *, int, bool);
extern void sys_recolor_cursor(repv face);
extern void sys_update_dimensions(WIN *);
extern GtkJade *sys_new_window(WIN *, WIN *, short *);
extern void sys_kill_window(WIN *);
extern int sys_set_font(WIN *);
extern void sys_unset_font(WIN *);
extern int sys_sleep_win(WIN *);
extern int sys_unsleep_win(WIN *);
extern void sys_activate_win(WIN *);
extern void sys_set_win_name(WIN *win, char *name);
extern void sys_set_win_pos(WIN *, long, long, long, long);
extern bool sys_deleting_window_would_exit (WIN *w);
extern void sys_windows_init(void);
extern void gtk_jade_foreach (GtkContainer *root,
			      GtkCallback fun, gpointer data);
extern repv Fgtk_jade_new (repv, repv);
extern repv Fgtk_jade_window (repv);
extern repv Fgtk_jade_window_widget (repv);
extern repv Fflush_output(void);

#elif defined (HAVE_X11)

/* from x11_keys.c */
extern void translate_event(u_long *, u_long *, XEvent *,
			    struct x11_display *);
extern int cook_key(void *, u_char *, int);
extern bool sys_lookup_mod(const char *name, u_long *mods);
extern bool sys_lookup_code(const char *name, u_long *code, u_long *mods);
extern char *sys_lookup_mod_name(char *buf, u_long mod);
extern bool sys_lookup_code_name(char *buf, u_long code, u_long type);
extern u_long x11_find_meta(struct x11_display *xd);
extern u_long esc_code, esc_mods;

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
extern WIN *x11_current_event_win;
extern long x11_current_mouse_x, x11_current_mouse_y;
extern Time x11_last_event_time;
extern struct x11_display *x11_display_list;
extern char **x11_argv;
extern int x11_argc;
extern bool x11_opt_reverse_video;

/* from x11_misc.c */
extern void sys_beep(WIN *w);
extern void x11_convert_selection(XSelectionRequestEvent *ev);
extern void x11_lose_selection(XSelectionClearEvent *ev);
extern void x11_window_lose_selections(WIN *w);
extern void x11_misc_init(void);
extern repv Qxa_primary, Qxa_secondary;
extern repv Fx11_set_selection(repv sel, repv start, repv end, repv buffer);
extern repv Fx11_selection_active_p(repv sel);
extern repv Fx11_own_selection_p(repv sel);
extern repv Fx11_get_selection(repv sel);
extern repv Fx11_lose_selection(repv sel);

/* from x11_windows.c */
extern int sys_sleep_win(WIN *);
extern int sys_unsleep_win(WIN *);
extern void sys_update_dimensions(WIN *);
extern void x11_update_dimensions(WIN *, int, int);
extern Window sys_new_window(WIN *, WIN *, short *);
extern void sys_kill_window(WIN *);
extern void sys_activate_win(WIN *);
extern void sys_set_win_name(WIN *win, char *name);
extern void sys_set_win_pos(WIN *, long, long, long, long);
extern WIN *x11_find_window(Window);
extern void sys_draw_glyphs(WIN *, int, int, glyph_attr, char *, int, bool);
extern int sys_set_font(WIN *);
extern void sys_unset_font(WIN *);
extern repv sys_get_mouse_pos(WIN *);
extern bool sys_deleting_window_would_exit (WIN *w);
extern void sys_windows_init(void);
extern repv Fflush_output(void);
extern repv Fmake_window_on_display(repv display);

#endif /* window system */

#endif /* JADE_SUBRS */
