(* Here are some extra programming problems that can be done using the material in this module. 
Many are similar in difficulty and content to the homework, but they are not the homework, so you are free to discuss 
solutions, etc. on the discussion forum. Thanks to Pavel Lepin and Charilaos Skiadas for contributing most of these. *)

(* 0 *)


(* 1 - 4 *)
(* Problems 1-4 use these type definitions: *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail
				
(* 1 *)
(* Write a function  pass_or_fail  of type {grade : int option, id : 'a} -> pass_fail  that takes a 
final_grade (or, as the type indicates, a more general type) and returns pass if the grade field contains 
SOME i for an i>= 75 (else fail). *)
fun pass_or_fail {id=x,grade=y} =
  case y of
      SOME i => if i >= 75 then pass else fail
    | NONE => fail 
  
(* 2 *)
(* Using pass_or_fail as a helper function, write a function has_passed of type {grade : int option, id : 'a} 
that returns true if and only if the the grade field contains SOME i for an i>=75 . *)
fun has_passed x =
      case pass_or_fail x of
	  pass => true
	| _ => false      

(* 3 *)
(* Using has_passed as a helper function, write a function number_passed that takes a 
list of type final_grade (or a more general type) and returns how many list elements have passing (again, >=75) grades. *)
(* (id * grade) list -> int *)
fun number_passed xs =
  case xs of
      [] => 0
    | y::yx => (if has_passed y then 1 else 0) + number_passed yx
								 
					 
(* 4 *)
(* Write a function number_misgraded of type (pass_fail * final_grade) list -> int that indicates how many list elements 
are 'mislabelled' where mislabelling means a pair (pass,x) where has_passed x is false or (fail,x) where has_passed x is true *)
(* (𝚙𝚊𝚜𝚜_𝚏𝚊𝚒𝚕 * 𝚏𝚒𝚗𝚊𝚕_𝚐𝚛𝚊𝚍𝚎) 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 *)				       
fun number_misgraded xs =
  case xs of
     [] => 0
   | (a,b)::yx => (if pass_or_fail b = a then 0 else 1) + number_misgraded yx

fun group_by_outcome xs =
  let
      fun loop (ys, passed_l, failed_l) =
		     case ys of
			 [] => (case (passed_l,failed_l) of
				    ([],[]) => []
				   |(a,[]) => [(pass,a)]
				   |([],b) => [(fail,b)]
				   |(a,b) => [(pass,a),(fail,b)])
		       | {id=x,grade=y}::ys' => (case pass_or_fail {id=x,grade=y} of
						     pass => loop (ys', passed_l @ [x],failed_l)
						   | fail => loop (ys', passed_l,failed_l @ [x]))
  in
      loop(xs,[],[])
  end
									   
(* 5 - 7 *)
(* Problems 5-7 use these type definitions: *)							       
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* 5 *)
(* Write a function tree_height that accepts an 'a tree and evaluates to a height of this tree. 
The height of a tree is the length of the longest path to a leaf. Thus the height of a leaf is 0. *)

fun tree_height x =
  case x of
     leaf => 0
   | node {value=a,left=b,right=c} => 1 + (if tree_height b > tree_height c then tree_height b else tree_height c)
	
(* 6 *)
(* Write a function sum_tree that takes an int tree and evaluates to the sum of all values in the nodes. *)
fun sum_tree x =
  case x of
     leaf => 0
   | node {value=a,left=b,right=c} => a + sum_tree b + sum_tree c

(* 7 *)
(* 𝚏𝚕𝚊𝚐 𝚝𝚛𝚎𝚎 -> 𝚏𝚕𝚊𝚐 𝚝𝚛𝚎𝚎 *)						
(* 'a tree * flag -> 'a tree * flag *)	
(* Write a function gardener of type flag tree -> flag tree such that its structure is identical to the original 
tree except all nodes of the input containing  are (along with all their descendants) replaced with a leaf. *)
fun gardener x = 
  case x of
      node {value=prune_me,left=b,right=c} => leaf
    | node {value=leave_me_alone,left=b,right=c} => node {value=leave_me_alone,left=gardener b,right=gardener c}
    | leaf => leaf
							    

(* flag -> flag *)	       
fun gardener_2 x =
  case x of
      prune_me => leave_me_alone
    | _ => x

	       
(* 8 *)
(* Re-implement various functions provided in the SML standard libraries for lists and options. Good examples include 𝚕𝚊𝚜𝚝, 𝚝𝚊𝚔𝚎, 𝚍𝚛𝚘𝚙, 𝚌𝚘𝚗𝚌𝚊𝚝, 𝚐𝚎𝚝𝙾𝚙𝚝, and 𝚓𝚘𝚒𝚗. *)

exception Empty
exception Subscript
	       
(* last *)	      
fun last l =
  case l of
      [] => raise Empty
    | x::[] => x
    | x::xs => last xs
		    
(* take *)
fun take (l,i) =
  if i < 0 orelse i > length l then raise Subscript
  else if i = length l then l				
  else
      let
	  fun loop (x::xs,n) =
	    if n = 0 then []
	    else x::loop(xs,n-1)
      in
	  loop(l,i)
      end

(* drop *)
fun drop (l,i) =
  if i < 0 orelse i > length l then raise Subscript
  else if i = length l then []
  else let
      fun loop (list, n)=
	case list of
	    [] => []
	  | x::xs => if n > 0 then loop (xs,n-1)
		     else x::loop(xs,n-1)
  in
      loop(l,i)
  end

(* concat *)
fun concat l =
  case l of
      [] => []
    | x::xs => x @ concat xs

(* getOpt *)
fun getOpt (opt,a) =
  case opt of
      SOME v => v
    | _ => a
	       
(* join *)
fun join opt =
  case opt of
      NONE => NONE
    | SOME (v) => v	    

		      
(* 9-16 *)
datatype nat = ZERO | SUCC of nat

(* 9 *)				  
fun is_positive x =
  case x of
      ZERO => false
    | SUCC i => true
(*  | _ => false *)

(* 10 *)		    
exception Negative
	      
fun pred x =
  case x of
      ZERO => raise Negative
    | SUCC i => i

(* 11 *)		    
fun nat_to_int x =
  case x of
      ZERO => 0
    | SUCC i => 1 + nat_to_int i

(* 12 *) 			       
fun int_to_nat x =
  if x < 0 then raise Negative
  else
      if x = 0 then ZERO else SUCC(int_to_nat (x-1)) 

(* 13 *)
fun add (x,y) =
  case x of
      ZERO => y
    | SUCC i => add (i,SUCC (y))
      
(* 14 *)
fun sub (x,y) =
  case (x,y) of
      (_,ZERO) => x
     |(ZERO, _) => raise Negative		      
      |(_,_) => sub (pred x, pred y)

(* 15 *)		   
fun mult (x,y) =
  case (x,y) of
      (ZERO,_) => ZERO
    | (_,ZERO) => ZERO
    | (SUCC ZERO,y) => y
    | (SUCC i,y) => mult (i, add(y,y))

(* 16 *)
fun less_then (x,y) =
  case (x,y) of
      (ZERO, SUCC i) => true
    | (_,ZERO) => false
    | (_,_) => less_then(pred x, pred y)
			
(* 17 - 19 *)
datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

(* Helpers *)
fun range_to_list (i,j) = if i > j then [] else i::range_to_list(i+1,j)
								
fun loop_list (xs,el) = case xs of
			    [] => false (* element is not in the list *)
			  | x::xs' => if x = el then true else loop_list (xs',el)
									
fun intersection_list (xs,ys) =
  case (xs,ys) of
      ([],_) => [] (* no intersection found, list empty *)
    | (_,[]) => [] (* no intersection found, list empty *)	    
    | (x::xs',y::ys') => let
	fun loop (el,list) =
	  case list of
	      [] => intersection_list(xs',ys)
	    | y::yx' => if x = y then y::loop(x,yx')
			else loop(x,yx')
      in
	  loop (x,ys)
      end

fun any_to_list x =
  case x of
      Elems xs => xs
    | Range {from=x1, to=y1} => range_to_list(x1,y1)
    | Union (y,z) => any_to_list y @ any_to_list z
    | Intersection (y,z) => intersection_list (any_to_list y, any_to_list z)

fun empty_list xs =
  case xs of
      [] => true
    | x::xs' => false
		    		    
(* 17 *)			       
fun isEmpty x =
  case x of
      Elems xs => empty_list xs
    | Range {from=x1, to=y1} => empty_list(range_to_list(x1,y1))
    | Union (y,z) => isEmpty y orelse isEmpty z
    | Intersection (y,z) => empty_list(intersection_list(any_to_list y, any_to_list z))
																		    
(* 18 *)		     
fun contains (x,el) =
  if isEmpty x then false
  else case x of
	   Elems xs => loop_list (xs,el)			 
	 | Range {from=x1, to=y1} => loop_list (range_to_list(x1,y1),el)
	 | Union (y,z) => contains(y,el) orelse contains(z,el)
	 | Intersection (y,z) => loop_list(intersection_list (any_to_list y, any_to_list z),el)

(* 19 *)					  
fun toList x =
  let fun no_duplicates xs =
	case xs of
	    [] => []
	  | x::xs' => if loop_list(xs',x) then no_duplicates xs' else x :: no_duplicates xs'
  in
      no_duplicates (any_to_list x)
  end
		    
 (* 0 *)
(* Consider any of the extra Practice Problems from Section 1 and redo them using pattern matching. *)

(* 0.1 *)
(* Write a function 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 that takes a list of numbers and adds them with alternating sign. For example 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 [𝟷,𝟸,𝟹,𝟺] = 𝟷 - 𝟸 + 𝟹 - 𝟺 = -𝟸. *)
fun alternate xs =
  case xs of
      [] => 0
   |  [x] => x			  
   | x::y::xs' => x - y + alternate xs'
				    
(* 0.2 *)
(* Write a function 𝚖𝚒𝚗_𝚖𝚊𝚡 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 that takes a non-empty list of numbers, and returns a pair (𝚖𝚒𝚗, 𝚖𝚊𝚡) of the minimum and maximum of the numbers in the list. *)				      
fun min_max xs =
  let fun min xs =
	case xs of
	   [] => NONE
	 | x::xs' => (let val min_xs = min xs'
		     in
			 case min_xs of
			     SOME min => if min < x then min_xs else SOME x
			   | NONE => SOME x
		     end)
      fun max xs =
	case xs of
	    [] => NONE
	  | x::xs' => (let val max_xs = max xs'
		      in
			  case max_xs of
			      SOME max => if max > x then max_xs else SOME x
			    | NONE => SOME x
		      end)
  in
      (min xs, max xs)
  end
      
(* 0.3 *)
(* Write a function 𝚌𝚞𝚖𝚜𝚞𝚖 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes a list of numbers and returns a list of the partial sums of those numbers. For example 𝚌𝚞𝚖𝚜𝚞𝚖 [𝟷,𝟺,𝟸𝟶] = [𝟷,𝟻,𝟸𝟻]. *)      
fun cumsum xs =
  case xs of
      [] => []
    | [x] => [x]
    | x::y::xs' => x :: cumsum(x + y :: xs')
			      
(* 0.4 *)
(* Write a function 𝚐𝚛𝚎𝚎𝚝𝚒𝚗𝚐 : 𝚜𝚝𝚛𝚒𝚗𝚐 𝚘𝚙𝚝𝚒𝚘𝚗 -> 𝚜𝚝𝚛𝚒𝚗𝚐 that given a string option 𝚂𝙾𝙼𝙴 name returns the string "𝙷𝚎𝚕𝚕𝚘 𝚝𝚑𝚎𝚛𝚎, ...!" where the dots would be replaced by name. Note that the name is given as an option, so if it is 𝙽𝙾𝙽𝙴 then replace the dots with "𝚢𝚘𝚞". *)			  
fun greeting x =
  case x of
      SOME s => "Hello there, " ^ s ^ "!"
    | NONE => "Hello there, you!"
		  
(* 0.5 *)
(* Write a function 𝚛𝚎𝚙𝚎𝚊𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. For example: 𝚛𝚎𝚙𝚎𝚊𝚝 ([𝟷,𝟸,𝟹], [𝟺,𝟶,𝟹]) = [𝟷,𝟷,𝟷,𝟷,𝟹,𝟹,𝟹]. *)	   
fun repeat (xs, ts) =
  case (xs,ts) of
      ([],_) => []
    | (_,[]) => []
    | (x::xs',t::ts') => if t = 0 then repeat(xs',ts')
			 else x :: repeat(xs,(t-1) :: ts')
					     
      
(* 0.6 *)
(* Write a function 𝚊𝚍𝚍𝙾𝚙𝚝 : 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 * 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that given two "optional" integers, adds them if they are both present (returning 𝚂𝙾𝙼𝙴 of their sum), or returns 𝙽𝙾𝙽𝙴 if at least one of the two arguments is 𝙽𝙾𝙽𝙴. *)
fun addOpt (x,y) =
  case (x,y) of
      (SOME x, SOME y) => SOME (x+y)
    | _ => NONE 

(* 7 *)
(* Write a function 𝚊𝚍𝚍𝙰𝚕𝚕𝙾𝚙𝚝 : 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that given a list of "optional" integers, adds those integers that are there (i.e. adds all the 𝚂𝙾𝙼𝙴 𝚒). For example: 𝚊𝚍𝚍𝙰𝚕𝚕𝙾𝚙𝚝 ([𝚂𝙾𝙼𝙴 𝟷, 𝙽𝙾𝙽𝙴, 𝚂𝙾𝙼𝙴 𝟹]) = 𝚂𝙾𝙼𝙴 𝟺. If the list does not contain any 𝚂𝙾𝙼𝙴 𝚒s in it, i.e. they are all 𝙽𝙾𝙽𝙴 or the list is empty, the function should return 𝙽𝙾𝙽𝙴. *)	   	   
fun addAllOpt xs =
  case xs of
      [] => NONE
    | x::xs' => let val  tl_sum = addAllOpt xs'
		in
		    case tl_sum of
			SOME sum => (case x of
					 SOME x => SOME (sum + x)
				       | NONE => tl_sum)
		      | NONE => (case x of
				     SOME x => SOME x
				   | NONE => NONE)
		end

