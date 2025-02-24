let ( let* ) = Tar.( let* )

let rec safe_read fd buf off len =
  try Unix.read fd buf off len
  with Unix.Unix_error (Unix.EINTR, _, _) -> safe_read fd buf off len

let rec run : type a. Unix.file_descr -> (a, _, _) Tar.t -> a = fun fd -> function
  | Tar.Read len ->
      let b = Bytes.create len in
      let read = safe_read fd b 0 len in
      if read = 0 then
        failwith "unexpected end of file"
      else if len = (read : int) then
        Bytes.unsafe_to_string b
      else
        Bytes.sub_string b 0 read
  | Tar.Really_read len ->
      let rec loop fd buf offset len =
        if offset < (len : int) then
          let n = safe_read fd buf offset (len - offset) in
          if n = 0 then
            failwith "unexpected end of file"
          else
            loop fd buf (offset + n) len
      in
      let buf = Bytes.create len in
      loop fd buf 0 len;
      Bytes.unsafe_to_string buf
  | Tar.Return (Ok x) -> x
  | Tar.Return (Error _) -> failwith "something's gone wrong"
  | Tar.High _ | Tar.Write _ | Tar.Seek _ -> assert false
  | Tar.Bind (x, f) -> run fd (f (run fd x))

let list fd =
  let go ?global:_ hdr () =
    let* content = Tar.really_read (Int64.to_int hdr.Tar.Header.file_size) in
    print_endline content;
    Tar.return (Ok ())
  in
  run fd (Tar_gz.in_gzipped (Tar.fold go ()))

let () =
  let filename = Sys.argv.(1) in
  list (Unix.openfile filename [Unix.O_RDONLY] 0)
