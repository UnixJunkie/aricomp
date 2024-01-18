
type token = int
type frequency = Q.t

(** [compute_frequencies tokens] compute the [token]
    to exact frequency mapping *)
val compute_frequencies: token list -> (token, frequency) BatHashtbl.t

(** [compute_byte_freqs_all_file filename] same as [compute_frequencies]
    but for a whole file *)
val compute_byte_freqs_all_file: string -> (token, frequency) BatHashtbl.t

(** [decr_sort_frequencies freqs] sort frequencies decreasingly *)
val decr_sort_frequencies: (token, frequency) BatHashtbl.t ->
  (token * frequency) list

(** [cumulate_frequencies_encoder freqs] prepare frequencies for efficient use
    by the encoder. WARNING: [freqs] MUST BE sorted decreasingly *)
val cumulate_frequencies_encoder: (token * frequency) list ->
  (token, (frequency * frequency)) BatHashtbl.t

(** [cumulate_frequencies_decoder freqs] prepare frequencies for efficient use
    by the decoder. WARNING: [freqs] MUST BE sorted decreasingly *)
val cumulate_frequencies_decoder: (token * frequency) list ->
  (token * (frequency * frequency)) list

(** [encode_one cum_freqs_enc left right tok]: encode one [tok].
    The current interval is \[left, right\[. *)
val encode_one: (token, frequency * frequency) Hashtbl.t ->
  frequency -> frequency -> token -> frequency * frequency

(** [encode_all cum_freqs tokens]: encode all [tokens] (the message).
    Return the interval \[left, right\[.
    Any float (left <= x < right) can be used to encode the message. *)
val encode_all: (token, frequency * frequency) Hashtbl.t ->
  token list -> frequency * frequency

(** [decode_one cum_freqs_dec left right enc_tok]: decode one [enc_tok].
    The current interval is \[left, right\[. *)
val decode_one: (token * (frequency * frequency)) list ->
  frequency -> token * frequency

(** [decode_all eol_tok cum_freqs_dec enc_msg]: decode [enc_msg].
    [eol_tok] is the token encoding End_of_message (for example, the token for "\n").
    [eol_tok] is required by the decoder to detect when to stop decoding. *)
val decode_all: token -> (token * (frequency * frequency)) list ->
  frequency -> token list
