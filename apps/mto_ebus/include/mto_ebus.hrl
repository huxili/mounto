
-record(mto_ebus_evt, {
              pid,      % Source pid or alias
              source,   % {app, module, fun}
              ts,
              dest,     % Opaque, can be a list of candidate or anyone (undefined)
              data      % Opaque
              }).