module Tar_gz = Tar_gz.Make
  (struct type 'a t = 'a
          let ( >>= ) x f = f x
          let return x = x end)
  (struct type out_channel = Stdlib.out_channel
          type 'a t = 'a
          let really_write oc cs =
            let str = Cstruct.to_string cs in
            output_string oc str end)
  (struct type in_channel = Stdlib.in_channel
          type 'a t = 'a
          let really_read ic cs =
            let len = Cstruct.length cs in
            let buf = Bytes.create len in
            really_input ic buf 0 len ;
            Cstruct.blit_from_bytes buf 0 cs 0 len
          let skip ic len =
            let buf = Bytes.create len in
            really_input ic buf 0 len
          let read ic cs =
            let max = Cstruct.length cs in
            let buf = Bytes.create max in
            let len = input ic buf 0 max in
            Cstruct.blit_from_bytes buf 0 cs 0 len ; len end)

let print ic hdr =
  let buf = Cstruct.create (Int64.to_int hdr.Tar.Header.file_size) in
  Tar_gz.really_read ic buf;
  print_endline (Cstruct.to_string buf)

let () =
  let ic = open_in Sys.argv.(1) in
  let ic = Tar_gz.of_in_channel ~internal:(Cstruct.create 4096) ic in
  let rec go global = match Tar_gz.get_next_header ~global ic with
    | hdr, global ->
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
    | exception Tar.Header.End_of_stream -> ()
  in
  go None
