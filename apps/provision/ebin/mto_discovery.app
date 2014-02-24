{application,mto_discovery,
             [{description,"Provision networks"},
              {vsn,"1"},
              {registered,[mto_upnp,mto_bonjour,mto_netbios]},
              {applications,[kernel,stdlib]},
              {mod,{mto_discovery,[]}},
              {env,[{mto_bonjour_trace,true}]},
              {modules,[emdns,emdns_test,mto_bonjour,mto_discovery,
                        mto_netbios,mto_trace,mto_upnp]}]}.
