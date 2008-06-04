{application, egitd,
  [{description, "The Erlang git-daemon"},
   {vsn, "0.0.0"},
   {modules, [egitd_app, egitd_sup, server]},
   {registered, [server]},
   {applications, [kernel, stdlib]},
   {mod, {egitd_app, []}},
   {start_phases, []}
  ]}.
