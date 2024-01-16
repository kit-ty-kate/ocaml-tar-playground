module Tar_gz = Tar_gz.Make
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (struct type out_channel = Stdlib.out_channel
          type 'a io = 'a
          let really_write = Stdlib.output_string end)
  (struct type in_channel = Stdlib.in_channel
          type 'a io = 'a
          let read ic buf = Stdlib.input ic buf 0 (Bytes.length buf) end)

let print ic hdr =
  let buf = Bytes.create (Int64.to_int hdr.Tar.Header.file_size) in
  Tar_gz.really_read ic buf;
  print_endline (Bytes.unsafe_to_string buf)

let () =
  let ic = open_in Sys.argv.(1) in
  let ic = Tar_gz.of_in_channel ~internal:(De.bigstring_create De.io_buffer_size) ic in
  let rec go global = match Tar_gz.HeaderReader.read ~global ic with
    | Ok (hdr, global) ->
        let data_length =
          if String.equal hdr.Tar.Header.file_name Sys.argv.(2) then begin
            print ic hdr;
            0 (* Data already read *)
          end else
            Int64.to_int hdr.Tar.Header.file_size
        in
        let data_padding = Tar.Header.compute_zero_padding_length hdr in
        Tar_gz.skip ic (data_length + data_padding);
        go global
    | Error (`Fatal _) -> failwith "malformed tar.gz file"
    | Error `Eof -> ()
  in
  go None
