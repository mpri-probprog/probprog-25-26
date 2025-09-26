open Byoppl
open Distribution
open Basic.Importance_sampling

let regression _prob _data =
  assert false

let data = []

let _ =
  Format.printf "@.-- Regressin, Basic Importance Sampling --@.";
  let a_dist, b_dist = split (infer regression data) in
  let a_m = mean a_dist in
  let b_m = mean b_dist in
  Format.printf "Regression: a=%f, b=%f@." a_m b_m
