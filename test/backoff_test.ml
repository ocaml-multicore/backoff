let test_basics () =
  let b = Backoff.create ~lower_wait_log:5 ~upper_wait_log:6 () in
  Alcotest.(check' int)
    ~msg:"initial is lower" ~expected:5 ~actual:(Backoff.get_wait_log b);
  let b = Backoff.once b in
  Alcotest.(check' int)
    ~msg:"incremented once" ~expected:6 ~actual:(Backoff.get_wait_log b);
  let b = Backoff.once b in
  Alcotest.(check' int)
    ~msg:"not incremented above upper" ~expected:6
    ~actual:(Backoff.get_wait_log b);
  let b = Backoff.reset b in
  Alcotest.(check' int)
    ~msg:"reset to lower" ~expected:5 ~actual:(Backoff.get_wait_log b)

let () =
  Alcotest.run "Backoff"
    [ ("basics", [ Alcotest.test_case "" `Quick test_basics ]) ]
