
type token = int
type frequency = Q.t

val compute_frequencies: token list -> (token, frequency) BatHashtbl.t

val compute_byte_freqs_all_file: string -> (token, frequency) BatHashtbl.t

val decr_sort_frequencies: (token, frequency) BatHashtbl.t ->
  (token * frequency) list

val cumulate_frequencies_encoder: (token * frequency) list ->
  (token, (frequency * frequency)) BatHashtbl.t

val cumulate_frequencies_decoder: (token * frequency) list ->
  (token * (frequency * frequency)) list

val encode_one: (token, frequency * frequency) Hashtbl.t ->
  frequency -> frequency -> token -> frequency * frequency

val encode_all: (token, frequency * frequency) Hashtbl.t ->
  token list -> frequency * frequency

val decode_one: (token * (frequency * frequency)) list ->
  frequency -> token * frequency

val decode_all: token -> (token * (frequency * frequency)) list ->
  frequency -> token list
