
module A = BatArray
module Ht = BatHashtbl
module L = BatList
module LO = Line_oriented  
module Log = Dolog.Log

type token = int

(* exact value from a "language model" *)
type frequency = Q.t

let compute_frequencies (tokens: token list): (token, frequency) Ht.t =
  let total = L.length tokens in
  let maxi = L.max tokens in
  let counts = A.make (maxi + 1) 0 in
  L.iter (fun tok ->
      counts.(tok) <- counts.(tok) + 1
    ) tokens;
  let res = Ht.create (maxi + 1) in
  A.iteri (fun tok count ->
      Ht.add res tok (Q.of_ints count total)
    ) counts;
  res

let ht_incr ht k =
  try Ht.replace ht k Z.(one + Ht.find ht k)
  with Not_found -> Ht.add ht k Z.one

let compute_byte_freqs_all_file fn =
  let res = Ht.create 256 in
  let total = ref Z.zero in
  for i = 0 to 255 do
    (* make any byte representable: init count is 1 instead of 0 *)
    Ht.add res i Z.one;
    total := Z.(!total + one)
  done;
  (* FBR:TODO file should be read into a buffer instead of line-by-line *)
  LO.iter fn (fun line ->
      String.iter (fun c ->
          ht_incr res (Char.code c);
          total := Z.(!total + one)
        ) line;
      (* EOL is skipped by input_line *)
      ht_incr res (Char.code '\n');
      total := Z.(!total + one)
    );
  (* count to frequencies *)
  Ht.map (fun _k v ->
      Q.make v !total
    ) res

(* most to least frequent token *)
let decr_sort_frequencies (freqs: (token, frequency) Ht.t):
  (token * frequency) list =
  L.sort (fun (t1, f1) (t2, f2) ->
      let cmp = Q.compare f2 f1 in
      if cmp = 0 then
        (* break ties by sorting tokens *)
        compare (t1: int) t2
      else
        cmp
    ) (Ht.to_list freqs)

(* sorted freqs to cumulative freqs;
   efficient data structure for the encoder
   WARNING: frequencies MUST BE decr. sorted *)
let cumulate_frequencies_encoder (freqs: (token * frequency) list):
  (token, frequency * frequency) Ht.t =
  let cum_freq = ref Q.zero in
  let res = Ht.create (L.length freqs) in
  L.iter (fun (tok, freq) ->
      let l_b = !cum_freq in
      cum_freq := Q.(!cum_freq + freq);
      Ht.add res tok (l_b, !cum_freq)
    ) freqs;
  assert(!cum_freq = Q.one);
  res

(* sorted freqs to cumulative freqs;
   efficient data structure for the decoder
   WARNING: frequencies MUST BE decr. sorted *)
let cumulate_frequencies_decoder freqs =
  let acc = ref Q.zero in
  let res =
    L.map (fun (tok, freq) ->
        let l_b = !acc in
        acc := Q.(!acc + freq);
        (tok, (l_b, !acc))
      ) freqs in
  assert(!acc = Q.one);
  res

let encode_one cum_freqs left right tok =
  let l_b, r_b = Ht.find cum_freqs tok in
  let delta = Q.(right - left) in
  Q.(left + (delta * l_b),
     left + (delta * r_b))

let encode_all cum_freqs tokens =
  L.fold_left (fun (left, right) tok ->
      encode_one cum_freqs left right tok
    ) (Q.zero, Q.one) tokens

let decode_one cum_freqs x =
  let rec loop = function
    | [] -> (Log.fatal "AC.decode_one: could not decode: %s" (Q.to_string x);
             exit 1)
    | (tok, (l_b, r_b)) :: xs ->
      if Q.(x < r_b) then
        let () = assert Q.(x >= l_b) in (* to be safe *)
        (* redress to the full [0, 1[ interval *)
        (tok, Q.((x - l_b) / (r_b - l_b)))
      else
        loop xs in
  loop cum_freqs

let decode_all eol cum_freqs x =
  let rec loop acc y =
    let tok, rem = decode_one cum_freqs y in
    (* Log.warn "tok: %d" tok; *)
    if tok = eol then
      L.rev (tok :: acc)
    else
      loop (tok :: acc) rem in
  loop [] x
