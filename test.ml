let ( let* ) = Tar.( let* )

let rec safe f a =
  try f a with
  | Unix.Unix_error (Unix.EINTR, _, _) -> safe f a

let run t fd =
  let rec run : type a. (a, _, _) Tar.t -> a = function
    | Tar.Write str ->
      ignore (safe (Unix.write_substring fd str 0) (String.length str))
    | Tar.Read len ->
      let b = Bytes.create len in
      let read = safe (Unix.read fd b 0) len in
      if read = 0 then
        failwith "Unexpected_end_of_file"
      else if len = (read : int) then
        Bytes.unsafe_to_string b
      else
        Bytes.sub_string b 0 read
    | Tar.Really_read len ->
      let rec loop fd buf offset len =
        if offset < (len : int) then
          let n = safe (Unix.read fd buf offset) (len - offset) in
          if n = 0 then
            failwith "Unexpected_end_of_file"
          else
            loop fd buf (offset + n) len
      in
      let buf = Bytes.create len in
      loop fd buf 0 len;
      Bytes.unsafe_to_string buf
    | Tar.Seek len -> ignore (safe (Unix.lseek fd len) Unix.SEEK_CUR)
    | Tar.Return (Ok x) -> x
    | Tar.Return (Error _) -> failwith "something's gone wrong"
    | Tar.High _ -> assert false
    | Tar.Bind (x, f) -> run (f (run x))
  in
  run t

let list fd =
  let go ?global:_ hdr () =
    let* content = Tar.really_read (Int64.to_int hdr.Tar.Header.file_size) in
    print_endline content;
    Tar.return (Ok ())
  in
  run (Tar_gz.in_gzipped (Tar.fold go ())) fd

let () =
  let filename = Sys.argv.(1) in
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
  list fd
