-define(Config(X, Y), ulitos_app:get_var(fyler_worker, X, Y)).

-record(file, {
  url :: string(),
  name :: string(),
  dir :: string(),
  is_aws :: string(),
  bucket :: string(),
  extension :: string(),
  size :: non_neg_integer(),
  tmp_path :: string(),
  target_dir = [] :: list()
}).

-record(task, {
  id :: non_neg_integer(),
  type :: atom(),
  category :: atom(),
  acl = public ::  atom(),
  priority = 1 :: atom() | pos_integer(),
  file :: #file{},
  callback = undefined :: binary(),
  worker :: reference(),
  meta = #{} :: #{},
  options = []
}).

-record(job_stats, {
  id = 0 :: non_neg_integer(),
  status = success :: success|failed,
  download_time = 0 :: non_neg_integer(),
  upload_time = 0 :: non_neg_integer(),
  file_size = 0 :: non_neg_integer(),
  file_path = "" :: string(),
  time_spent = 0 :: non_neg_integer(),
  result_path = [] :: string(),
  task_type = do_nothing :: atom(),
  error_msg = "" :: string(),
  ts = 0 :: non_neg_integer(),
  url = []:: string(),
  options,
  priority = "" :: string()
}).

-type event_type() ::complete|failed|cpu_high|cpu_available.

-record(fevent, {
  type :: event_type(),
  node :: atom(),
  category :: atom(),
  task :: #task{},
  stats :: #job_stats{},
  error = undefined ::any()
}).

-record(video_info,{
  audio_codec = undefined ::string(),
  video_codec = undefined ::string(),
  video_size = undefined ::string(),
  video_bad_size = false ::atom(),
  pixel_format = undefined ::string()
}).