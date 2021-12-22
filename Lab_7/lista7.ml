(*Rafał Kruszyna*)
module type QUEUE_FUN =
sig
  (* Module [QueueFun]: first-in first-out queues *)

  (* This module implements queues (FIFOs)in a functional way. *)

  type 'a t
        (* The type of queues containing elements of type ['a]. *)
  exception Empty of string
        (* Raised when [first] is applied to an empty queue. *)
  val empty: unit -> 'a t
        (* Return a new queue, initially empty. *)
  val enqueue: 'a * 'a t -> 'a t
        (* [enqueue x q] adds the element [x] at the end of queue [q]. *)
  val dequeue: 'a t -> 'a t
        (* [dequeue q] removes the first element in queue [q] *)
  val first: 'a t -> 'a
        (* [first q] returns the first element in queue [q] without removing
           it from the queue, or raises [Empty] if the queue is empty.*)
  val isEmpty: 'a t -> bool
        (* [isEmpty q] returns [true] if queue [q] is empty,
           otherwise returns [false]. *)
end;;

(*Zadanie 1a *)
module QueueList: QUEUE_FUN=
    struct
        type 'a t =  'a list
         exception Empty of string

        let empty() =  []
        let enqueue(elem, queue) = queue @ [elem]
        let dequeue(queue) =
          match queue with
          | [] -> []
          | _::t -> t

        let first(queue) =
          match queue with
          | [] -> raise (Empty "Pusta kolejka")
          | h::_ -> h

        let isEmpty(queue) =
          queue = []
    end;;
(*Zadanie 1b *)
module QueuePairOfList: QUEUE_FUN=
    struct
         type 'a t = 'a list * 'a list
            exception Empty of string

            let empty() =
              ([], [])
            let normalRepresentation(queue) =
              match queue with
              | ([], listEnd) -> (List.rev listEnd, [])
              | normalQueue -> normalQueue
            let enqueue(elem, (listBeginning, listEnd)) = normalRepresentation(listBeginning, elem :: listEnd)

            let dequeue(listBeginning, listEnd) =
              match (listBeginning, listEnd) with
              | ([], _) -> ([],[])
              | (_ :: t, listEnd) -> normalRepresentation(t, listEnd)

            let first(listBeginning, listEnd) =
              match (listBeginning, listEnd) with
              | ([], _) -> raise(Empty "Kolejka pusta")
              | (h :: _, _) -> h

            let isEmpty(listBeginning, listEnd) =
              listBeginning = []
        end;;
(*Zadanie 2*)
module type QUEUE_MUT =
sig
    type 'a t
        (* The type of queues containing elements of type ['a]. *)
    exception Empty of string
        (* Raised when [first q] is applied to an empty queue [q]. *)
    exception Full of string
        (* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
    val empty: int -> 'a t
        (* [empty n] returns a new queue of length [n], initially empty. *)
    val enqueue: 'a * 'a t -> unit
        (* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
    val dequeue: 'a t -> unit
        (* [dequeue q] removes the first element in queue [q] *)
    val first: 'a t -> 'a
        (* [first q] returns the first element in queue [q] without removing
        it from the queue, or raises [Empty] if the queue is empty. *)
    val isEmpty: 'a t -> bool
        (* [isEmpty q] returns [true] if queue [q] is empty,
        otherwise returns [false]. *)
    val isFull: 'a t -> bool
        (* [isFull q] returns [true] if queue [q] is full,
        otherwise returns [false]. *)
end;;

module QueueCyclic: QUEUE_MUT=
    struct
            type 'a t = { mutable f: int; mutable r: int; mutable arr : 'a option array }
            exception Empty of string
            exception Full of string

            let empty n = {f = 0; r = 0; arr = Array.make (n + 1) None}



            let isFull q = (q.f - q.r = 1) || q.r - q.f = Array.length q.arr - 1
            let isEmpty q = q.r = q.f

            let enqueue (x, q) =
              if isFull q then raise(Full "Kolejka pelna")
              else begin
                q.arr.(q.r) <- Some x;
                if q.r >= Array.length q.arr - 1 then q.r <- 0
                else q.r <- q.r + 1
                end

            let dequeue q =
              if isEmpty q then ()
              else begin
                if q.f >= Array.length q.arr - 1 then q.f <- 0
                else q.f <- q.f + 1
                end

            let first q = if isEmpty q then raise(Empty "Kolejka pusta")
                          else match q.arr.(q.f) with
                               | Some element -> element
                               | None -> failwith "Element None"
    end;;

