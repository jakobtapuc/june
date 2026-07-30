signature JUNE_ERROR_REPORTER =
sig
  val reportFromMachine: string
                         -> Machine.runtime_error
                         -> Machine.trace
                         -> string

  val report: string -> exn -> string
end
