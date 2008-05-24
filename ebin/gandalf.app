{application, gandalf,
  [{description, "The Erlang git-daemon"},
   {vsn, "0.0.0"},
   {modules, [gandalf_app, gandalf_sup, server]},
   {registered, [server]},
   {applications, [kernel, stdlib]},
   {mod, {gandalf_app, []}},
   {start_phases, []}
  ]}.
