-define(Config(X, Y), ulitos_app:get_var(fyler_worker, X, Y)).

-record(task, {
  id = 0 :: non_neg_integer(),
  type :: atom(),
  category :: atom(),
  source = #{} :: map(),
  output = #{} :: map(),
  name = <<"">> :: binary(),
  extension = <<"">> :: binary(),
  options  = #{} :: map(),
  result = #{} :: map(),
  stats = #{} :: map(),
  local_dir = "" :: string(),
  size = 0 :: non_neg_integer()
}).

-type event_type() :: downloading | processing | uploading | completed | error | aborted.

-record(fevent, {
  type :: event_type(),
  task :: #task{},
  error = undefined :: any()
}).