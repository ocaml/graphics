/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include "libgraph.h"
#include <caml/alloc.h>

XftFont * caml_gr_font = NULL;

static void caml_gr_get_font(const char *fontname)
{
  XftFont * font;

  /* Is it a X Logical Font Description? LFDs start with a '-' character.
   * If not, use nicer FontConfig names like "Courier-12".
   */
  if (fontname[0] == '-')
    font = XftFontOpenXlfd (caml_gr_display, caml_gr_screen, fontname);
  else
    font = XftFontOpenName (caml_gr_display, caml_gr_screen, fontname);
  if (font == NULL) caml_gr_fail("cannot find font %s", fontname);
  if (caml_gr_font != NULL) XftFontClose (caml_gr_display, caml_gr_font);
  caml_gr_font = font;
}

value caml_gr_set_font(value fontname)
{
  caml_gr_check_open();
  caml_gr_get_font(String_val(fontname));
  return Val_unit;
}

value caml_gr_set_text_size (value sz)
{
  return Val_unit;
}

static void caml_gr_draw_text(const char *txt, int len)
{
  int rgb;
  XftColor xftcol;
  XftDraw *d;
  Visual *visual = DefaultVisual (caml_gr_display, caml_gr_screen);
  XGlyphInfo info;

  rgb = caml_gr_rgb_pixel (caml_gr_color);
  xftcol.pixel = rgb;
  /* The range of these fields is 0..0xffff */
  xftcol.color.red = rgb >> 8;;
  xftcol.color.green = (rgb & 0xff00);
  xftcol.color.blue = (rgb & 0xff) << 8;
  xftcol.color.alpha = 0xffff;

  if (caml_gr_font == NULL) caml_gr_get_font(DEFAULT_FONT);
  if (caml_gr_remember_modeflag) {
    d = XftDrawCreate (caml_gr_display, caml_gr_bstore.win,
          visual, caml_gr_colormap);
    XftDrawString8 (d, &xftcol, caml_gr_font,
        caml_gr_x, Bcvt(caml_gr_y) - caml_gr_font->descent + 1,
        (const FcChar8 *) txt, len);
  }
  if (caml_gr_display_modeflag) {
    d = XftDrawCreate (caml_gr_display, caml_gr_window.win,
          visual, caml_gr_colormap);
    XftDrawString8 (d, &xftcol, caml_gr_font,
        caml_gr_x, Wcvt(caml_gr_y) - caml_gr_font->descent + 1,
        (const FcChar8 *) txt, len);
    XFlush(caml_gr_display);
  }

  if (d) XftDrawDestroy (d);

  XftTextExtents8 (caml_gr_display, caml_gr_font,
      (const FcChar8 *) txt, len, &info);
  caml_gr_x += info.width;
}

value caml_gr_draw_char(value chr)
{
  char str[1];
  caml_gr_check_open();
  str[0] = Int_val(chr);
  caml_gr_draw_text(str, 1);
  return Val_unit;
}

value caml_gr_draw_string(value str)
{
  caml_gr_check_open();
  caml_gr_draw_text(String_val(str), caml_string_length(str));
  return Val_unit;
}

value caml_gr_text_size(value str)
{
  XGlyphInfo info;
  value res;
  caml_gr_check_open();
  if (caml_gr_font == NULL) caml_gr_get_font(DEFAULT_FONT);
  XftTextExtents8 (caml_gr_display, caml_gr_font,
      (const FcChar8 *) (String_val(str)), caml_string_length(str), &info);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_int(info.width);
  Field(res, 1) = Val_int(info.height);
  return res;
}
