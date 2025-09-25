module Rejection_sampling_hard = struct
  exception Reject

  type prob = Prob

  let sample _prob _d = assert false
  let assume _prob _p = assert false
  let observe _prob _d _x = assert false

  let infer ?(_n = 1000) _model _obs =
    assert false
end

module Importance_sampling = struct
  type prob = { mutable score : float }

  let sample _prob _d = assert false
  let factor _prob _w = assert false
  let assume _prob _p = assert false
  let observe _prob _d _x = assert false

  let infer ?(_n = 1000) _model _obs =
    assert false
end