
#define CAML_INTERNALS
#ifndef CAML_NAME_SPACE
#define CAML_NAME_SPACE
#endif
/* If CAML_NAME_SPACE is not defined, then legacy names like
   [backtrace_last_exn] are in scope, which can lead to confusing errors.
   It's cleaner to disable those names. */
#include <caml/backtrace.h>
#include <caml/mlvalues.h>

CAMLprim value Base_lite_clear_caml_backtrace_pos() {
  caml_backtrace_pos = 0;
  return Val_unit;
}
