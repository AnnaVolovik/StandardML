(* Extra practice problems *)

(* Here are some extra programming problems that can be done using the material in this module. Many are similar in difficulty and content to the homework, but they are not the homework, so you are free to discuss solutions, etc. on the discussion forum. Thanks to Charilaos Skiadas for contributing these. *)


(* 1 *)
(* Write a function 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 that takes a list of numbers and adds them with alternating sign. For example 𝚊𝚕𝚝𝚎𝚛𝚗𝚊𝚝𝚎 [𝟷,𝟸,𝟹,𝟺] = 𝟷 - 𝟸 + 𝟹 - 𝟺 = -𝟸. *)

fun alternate xs =
  if null xs then 0
  else if null (tl xs) then hd xs
  else hd xs - hd (tl xs) + alternate (tl (tl xs))
					
(* 2 *)
(* Write a function 𝚖𝚒𝚗_𝚖𝚊𝚡 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 that takes a non-empty list of numbers, and returns a pair (𝚖𝚒𝚗, 𝚖𝚊𝚡) of the minimum and maximum of the numbers in the list. *)				      
fun min_max xs =
  let
      fun min xs =
	if null xs then NONE
	else
	    let
		val min_xs = min (tl xs)
	    in
     		if isSome min_xs andalso valOf min_xs < hd xs then min_xs
		else SOME (hd xs)
	    end
      fun max xs =
	if null xs then NONE
	else
	    let
		val max_xs = max (tl xs)
	    in
		if isSome max_xs andalso valOf max_xs > hd xs then max_xs
		else SOME (hd xs)
	    end
  in
      (min xs, max xs)
  end

(* 3 *)
(* Write a function 𝚌𝚞𝚖𝚜𝚞𝚖 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes a list of numbers and returns a list of the partial sums of those numbers. For example 𝚌𝚞𝚖𝚜𝚞𝚖 [𝟷,𝟺,𝟸𝟶] = [𝟷,𝟻,𝟸𝟻]. *)      
fun cumsum xs =
  if null xs then []
  else if null (tl xs) then [hd xs]
  else (hd xs) :: cumsum (hd xs + hd (tl xs) :: (tl (tl xs)))
      
      
(* 4 *)
(* Write a function 𝚐𝚛𝚎𝚎𝚝𝚒𝚗𝚐 : 𝚜𝚝𝚛𝚒𝚗𝚐 𝚘𝚙𝚝𝚒𝚘𝚗 -> 𝚜𝚝𝚛𝚒𝚗𝚐 that given a string option 𝚂𝙾𝙼𝙴 name returns the string "𝙷𝚎𝚕𝚕𝚘 𝚝𝚑𝚎𝚛𝚎, ...!" where the dots would be replaced by name. Note that the name is given as an option, so if it is 𝙽𝙾𝙽𝙴 then replace the dots with "𝚢𝚘𝚞". *)			  
fun greeting x =
  if isSome x then "Hello there, " ^ valOf x ^ "!"
  else "Hello there, you!"

(* 5 *)
(* Write a function 𝚛𝚎𝚙𝚎𝚊𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. For example: 𝚛𝚎𝚙𝚎𝚊𝚝 ([𝟷,𝟸,𝟹], [𝟺,𝟶,𝟹]) = [𝟷,𝟷,𝟷,𝟷,𝟹,𝟹,𝟹]. *)	   
fun repeat (xs, t) =
  if null xs orelse null t then []
  else
      if hd t = 0 then repeat (tl xs, tl t)
      else hd xs :: repeat (xs, (hd t - 1) :: tl t)
			   
(* 6 *)	  
(* Write a function 𝚊𝚍𝚍𝙾𝚙𝚝 : 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 * 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that given two "optional" integers, adds them if they are both present (returning 𝚂𝙾𝙼𝙴 of their sum), or returns 𝙽𝙾𝙽𝙴 if at least one of the two arguments is 𝙽𝙾𝙽𝙴. *)
fun addOpt (x,y) =
  if isSome x andalso isSome y then SOME (valOf x + valOf y)
  else NONE

(* 7 *)
(* Write a function 𝚊𝚍𝚍𝙰𝚕𝚕𝙾𝚙𝚝 : 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that given a list of "optional" integers, adds those integers that are there (i.e. adds all the 𝚂𝙾𝙼𝙴 𝚒). For example: 𝚊𝚍𝚍𝙰𝚕𝚕𝙾𝚙𝚝 ([𝚂𝙾𝙼𝙴 𝟷, 𝙽𝙾𝙽𝙴, 𝚂𝙾𝙼𝙴 𝟹]) = 𝚂𝙾𝙼𝙴 𝟺. If the list does not contain any 𝚂𝙾𝙼𝙴 𝚒s in it, i.e. they are all 𝙽𝙾𝙽𝙴 or the list is empty, the function should return 𝙽𝙾𝙽𝙴. *)	   	   
fun addAllOpt xs =
  if null xs then NONE
  else
      let
	  val tl_sum = addAllOpt (tl xs)
      in
	  if isSome (hd xs) then SOME (valOf (hd xs) + (if isSome tl_sum then valOf tl_sum else 0))
	  else tl_sum
      end	  
      
(* 8 *)
(* Write a function 𝚊𝚗𝚢 : 𝚋𝚘𝚘𝚕 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕 that given a list of booleans returns 𝚝𝚛𝚞𝚎 if there is at least one of them that is 𝚝𝚛𝚞𝚎, otherwise returns 𝚏𝚊𝚕𝚜𝚎. (If the list is empty it should return 𝚏𝚊𝚕𝚜𝚎 because there is no 𝚝𝚛𝚞𝚎.) *)	
fun any xs =
  if null xs then false
  else hd xs = true orelse any (tl xs)

(* 9 *)
(* Write a function 𝚊𝚕𝚕 : 𝚋𝚘𝚘𝚕 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕 that given a list of booleans returns 𝚝𝚛𝚞𝚎 if all of them 𝚝𝚛𝚞𝚎, otherwise returns 𝚏𝚊𝚕𝚜𝚎. (If the list is empty it should return 𝚝𝚛𝚞𝚎 because there is no 𝚏𝚊𝚕𝚜𝚎.) *)			       
fun all xs =
  if null xs then true
  else hd xs = true andalso all (tl xs) = true
					      
(* 10 *)
(* Write a function 𝚣𝚒𝚙 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 list that given two lists of integers creates consecutive pairs, and stops when one of the lists is empty. For example: 𝚣𝚒𝚙 ([𝟷,𝟸,𝟹], [𝟺, 𝟼]) = [(𝟷,𝟺), (𝟸,𝟼)]. *)			
fun zip (xs : int list, ys : int list) =
  if null xs orelse null ys then []
  else (hd xs, hd ys) :: zip (tl xs, tl ys)

(* 11 *)
(* Challenge: Write a version 𝚣𝚒𝚙𝚁𝚎𝚌𝚢𝚌𝚕𝚎 of 𝚣𝚒𝚙, where when one list is empty it starts recycling from its start until the other list completes. For example: 𝚣𝚒𝚙𝚁𝚎𝚌𝚢𝚌𝚕𝚎 ([𝟷,𝟸,𝟹], [𝟷, 𝟸, 𝟹, 𝟺, 𝟻, 𝟼, 𝟽]) = [(𝟷,𝟷), (𝟸,𝟸), (𝟹, 𝟹), (𝟷,𝟺), (𝟸,𝟻), (𝟹,𝟼), (𝟷,𝟽)]. *)			     
fun zipRecycle (xs, ys) =
  let
      fun cycle (ls,n) =
	if n = 0 then []
	else ls @ cycle (ls, n - 1)
      val lx = length xs
      val ly = length ys
  in
      if lx > ly then zip (xs, cycle (ys, (lx div ly) + 1))
      else zip (cycle (xs, (ly div lx) + 1), ys)
  end				  
	  
	  
(* 12 *)
(* Lesser challenge: Write a version 𝚣𝚒𝚙𝙾𝚙𝚝 of 𝚣𝚒𝚙 with return type (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 𝚘𝚙𝚝𝚒𝚘𝚗. This version should return 𝚂𝙾𝙼𝙴 of a list when the original lists have the same length, and 𝙽𝙾𝙽𝙴 if they do not. *)      
fun zipOpt (xs, ys) =
  let
      fun checker (xs, ys) =
	if null xs andalso null ys then true
	else if null xs orelse null ys then false
	else checker (tl xs, tl ys)
  in
      if checker (xs, ys) then SOME (zip (xs, ys)) else NONE
  end

fun zipOpt2 (xs : int list, ys : int list) =
  if null xs andalso null ys then SOME []
  else if null xs orelse null ys then NONE
  else if isSome (zipOpt2 (tl xs, tl ys)) then SOME ((hd xs, hd ys) :: valOf (zipOpt2(tl xs, tl ys))) else NONE

fun zipOpt3 (xs,ys) =
  if length xs = length ys then SOME (zip (xs, ys))
  else NONE
			      
      
(* 13 *)
(* Write a function 𝚕𝚘𝚘𝚔𝚞𝚙 : (𝚜𝚝𝚛𝚒𝚗𝚐 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 * 𝚜𝚝𝚛𝚒𝚗𝚐 -> 𝚒𝚗𝚝 𝚘𝚙𝚝𝚒𝚘𝚗 that takes a list of pairs (𝚜, 𝚒) and also a string 𝚜𝟸 to look up. It then goes through the list of pairs looking for the string 𝚜𝟸 in the first component. If it finds a match with corresponding number 𝚒, then it returns 𝚂𝙾𝙼𝙴 𝚒. If it does not, it returns 𝙽𝙾𝙽𝙴. *)	   
fun lookup (xs : (string * int) list , s2 : string) =
  if null xs then NONE
  else
      if #1 (hd xs) = s2 then SOME (#2 (hd xs))
      else lookup (tl xs, s2)
		  
  
(* 14 *)
(* Write a function 𝚜𝚙𝚕𝚒𝚝𝚞𝚙 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a list of integers creates two lists of integers, one containing the non-negative entries, the other containing the negative entries. Relative order must be preserved: All non-negative entries must appear in the same order in which they were on the original list, and similarly for the negative entries. *)		  
fun splitup xs =
  let
      fun helper (pl, nl, xs) =
	if null xs then (pl, nl)
	else
	    if hd xs >= 0 then helper ( hd xs :: pl, nl, tl xs)
	    else helper (pl, hd xs :: nl, tl xs)
  in
      helper ([],[],xs)
  end

fun splitup2 xs =
  if null xs then ([],[])
  else
      let
	  val (ps,ns) = splitup2 (tl xs)
      in
	  if hd xs >= 0 then (hd xs :: ps, ns)
	  else (ps, hd xs :: ns)
      end			     
      
(* 15 *)
(* Write a version 𝚜𝚙𝚕𝚒𝚝𝙰𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 of the previous function that takes an extra "threshold" parameter, and uses that instead of 0 as the separating point for the two resulting lists. *)	  
fun splitAt (xs, i) =
  let
      fun helper (pl, nl, xs) =
	if null xs then (pl, nl)
	else
	    if hd xs < i then helper ( pl @ [hd xs], nl, tl xs)
	    else helper (pl, nl @ [hd xs], tl xs)
  in
      helper ([],[],xs)
  end
		   
(* 16 *)
(* Write a function 𝚒𝚜𝚂𝚘𝚛𝚝𝚎𝚍 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕𝚎𝚊𝚗 that given a list of integers determines whether the list is sorted in increasing order. *)
fun isSorted xs =
  if null xs orelse  null (tl xs) then true
  else hd xs <= hd (tl xs) andalso isSorted (tl xs)
      
      
(* 17 *)
(* Write a function 𝚒𝚜𝙰𝚗𝚢𝚂𝚘𝚛𝚝𝚎𝚍 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚋𝚘𝚘𝚕𝚎𝚊𝚗, that given a list of integers determines whether the list is sorted in either increasing or decreasing order. *)					    
fun isAnySorted xs =
  if null xs orelse null (tl xs) then true
  else if hd xs < hd (tl xs) then isSorted (tl xs)
  else if hd xs = hd (tl xs) then isAnySorted (tl xs)			   
  else
      let
	  fun helper (xs) =
	    if null xs orelse  null (tl xs) then true
	    else hd xs >= hd (tl xs) andalso helper (tl xs)
      in
	  helper (xs)
      end

(* 18 *)
(* Write a function 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes two lists of integers that are each sorted from smallest to largest, and merges them into one sorted list. For example: 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎 ([𝟷,𝟺,𝟽], [𝟻,𝟾,𝟿]) = [𝟷,𝟺,𝟻,𝟽,𝟾,𝟿]. *)	  
fun sortedMerge (xs, ys) =
  if null xs then ys
  else if null ys then xs
  else
      if hd xs < hd ys then hd xs :: sortedMerge(tl xs, ys)
      else hd ys :: sortedMerge(xs, tl ys)
			    
(* 19 *)
(* Write a sorting function 𝚚𝚜𝚘𝚛𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that works as follows: Takes the first element out, and uses it as the "threshold" for 𝚜𝚙𝚕𝚒𝚝𝙰𝚝. It then recursively sorts the two lists produced by 𝚜𝚙𝚕𝚒𝚝𝙰𝚝. Finally it brings the two lists together. (Don't forget that element you took out, it needs to get back in at some point). You could use 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎 for the "bring together" part, but you do not need to as all the numbers in one list are less than all the numbers in the other.) *)			       
fun qsort xs =
  if null xs orelse null (tl xs) then xs
  else
      let
	  val first_list = (#1 (splitAt (tl xs, hd xs)))
	  val second_list = #2 (splitAt (tl xs, hd xs))
      in
	  qsort first_list @  [hd xs] @  qsort second_list
      end
	  
     
(* 20 *)
(* Write a function 𝚍𝚒𝚟𝚒𝚍𝚎 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 * 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that takes a list of integers and produces two lists by alternating elements between the two lists. For example: 𝚍𝚒𝚟𝚒𝚍𝚎 ([𝟷,𝟸,𝟹,𝟺,𝟻,𝟼,𝟽]) = ([𝟷,𝟹,𝟻,𝟽], [𝟸,𝟺,𝟼]). *)	  
fun divide xs =
  if null xs then ([],[])
  else if null (tl xs) then ([hd xs],[])
  else
      let val (f,s) = divide (tl(tl xs))
      in
	  (hd xs :: f, hd (tl xs) :: s)
      end
	        

(* 21 *)
(* Write another sorting function 𝚗𝚘𝚝_𝚜𝚘_𝚚𝚞𝚒𝚌𝚔_𝚜𝚘𝚛𝚝 : 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that works as follows: Given the initial list of integers, splits it in two lists using divide, then recursively sorts those two lists, then merges them together with 𝚜𝚘𝚛𝚝𝚎𝚍𝙼𝚎𝚛𝚐𝚎. *)	  
fun not_so_quick_sort xs =
  if null xs orelse null (tl xs) then xs
  else      
      let
	  val first_list = #1 (divide xs)
	  val second_list = #2 (divide xs)
      in
	  sortedMerge (not_so_quick_sort first_list, not_so_quick_sort second_list)
      end
      
(* 22 *)
(* Write a function 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎 : 𝚒𝚗𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝 * 𝚒𝚗𝚝 that given two numbers 𝚔 and 𝚗 it attempts to evenly divide 𝚔 into 𝚗 as many times as possible, and returns a pair (𝚍, 𝚗𝟸) where 𝚍 is the number of times while 𝚗𝟸 is the resulting 𝚗 after all those divisions. Examples: 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎 (𝟸, 𝟺𝟶) = (𝟹, 𝟻) because 𝟸*𝟸*𝟸*𝟻 = 𝟺𝟶 and 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎((𝟹,𝟷𝟶)) = (𝟶, 𝟷𝟶)  because 𝟹 does not divide 𝟷𝟶. *)
fun fullDivide (n,k) =
  let
      fun middle_fun (k, n, d) =
	if k mod n = 0 then  middle_fun (k div n, n, d + 1)
	else (d, k)
  in
      middle_fun (k, n, 0)
  end
      
(* 23 *)
(* Using 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎, write a function 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎 : 𝚒𝚗𝚝 -> (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 that given a number 𝚗 returns a list of pairs (𝚍, 𝚔) where 𝚍 is a prime number dividing 𝚗 and 𝚔 is the number of times it fits. The pairs should be in increasing order of prime factor, and the process should stop when the divisor considered surpasses the square root of 𝚗. If you make sure to use the reduced number 𝚗𝟸 given by 𝚏𝚞𝚕𝚕𝙳𝚒𝚟𝚒𝚍𝚎 for each next step, you should not need to test if the divisors are prime: If a number divides into 𝚗, it must be prime (if it had prime factors, they would have been earlier prime factors of 𝚗 and thus reduced earlier). Examples: 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎(𝟸𝟶) = [(𝟸,𝟸), (𝟻,𝟷)]; 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎(𝟹𝟼) = [(𝟸,𝟸), (𝟹,𝟸)]; 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎(𝟷) = []. *)
      
fun factorize x =
  let
      fun check_prime n =
	let
	    fun check_prime_inner (n, i) =
	      if i = 1 then true
	      else (n mod i) <> 0 andalso check_prime_inner (n, i - 1)
	in
	    check_prime_inner (n, n-1)
	end	    
      fun factorize_inner (x, divisor) =
	if x = 1 then []
	else if divisor = x orelse real divisor > Math.sqrt (real x) then [(x,1)]
	else
	    if check_prime divisor (* check is divisor a prime *)
	    then
		let
		    val possible_divisor = fullDivide (divisor, x)
		in
		    (* if we are able to divide then attach that pair and keep going *)
		    if #1 possible_divisor > 0 then (divisor, #1 possible_divisor) :: factorize_inner(#2 possible_divisor, divisor + 1)
		    (* cannot divide on this prime number, checking the next one *)		      
		    else factorize_inner (x, divisor + 1)
		end
	    else factorize_inner (x, divisor + 1)
  in
      factorize_inner (x, 2)
  end
      
(* 24 *)
(* Write a function 𝚖𝚞𝚕𝚝𝚒𝚙𝚕𝚢 : (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 that given a factorization of a number 𝚗 as described in the previous problem computes back the number 𝚗. So this should do the opposite of 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎. *)
fun multiply (xs : (int * int) list) =
  if null xs then 1
  else
      let
	  fun inner (number, times) =
	    if times = 1 then number
	    else number * inner (number, times - 1)
      in
	  inner (hd xs) * multiply (tl xs)
      end
	  
(* 25 *)
(* Challenge (hard): Write a function 𝚊𝚕𝚕_𝚙𝚛𝚘𝚍𝚞𝚌𝚝𝚜 : (𝚒𝚗𝚝 * 𝚒𝚗𝚝) 𝚕𝚒𝚜𝚝 -> 𝚒𝚗𝚝 𝚕𝚒𝚜𝚝 that given a factorization list result from 𝚏𝚊𝚌𝚝𝚘𝚛𝚒𝚣𝚎 creates a list all of possible products produced from using some or all of those prime factors no more than the number of times they are available. This should end up being a list of all the divisors of the number 𝚗 that gave rise to the list. Example: 𝚊𝚕𝚕_𝚙𝚛𝚘𝚍𝚞𝚌𝚝𝚜([(𝟸,𝟸), (𝟻,𝟷)]) = [𝟷,𝟸,𝟺,𝟻,𝟷𝟶,𝟸𝟶]. For extra challenge, your recursive process should return the numbers in this order, as opposed to sorting them afterwards. *)
fun remove_duplicates xs =
  if null xs then []
  else
      let
	  fun no_duplicates (x, xs) =
	    if null xs then true
	    else if x = hd xs then false
	    else no_duplicates (x, tl xs)	
      in
	  if no_duplicates (hd xs, tl xs) then (hd xs) ::  remove_duplicates (tl xs)
	  else remove_duplicates (tl xs)
      end
					  
fun one_pair_options (x, y) =
  let
      fun r_call_one_pair (st, t, c) =
	if t = 0 then []			  
	else c :: r_call_one_pair (st, t - 1, c * st)
  in
      r_call_one_pair (x, y, x)
  end

fun t2_to_0 (one : (int * int), two : (int * int)) =
  let fun t2_countdown (st1, t1, st2, t2, c) =
	if t2 = 0 then [c] (* it will include the last multiplication we made on a previous step *)
	else c ::  t2_countdown (st1, t1, st2, t2 - 1, c * st2)
  in
      t2_countdown (#1 one, #2 one, #1 two, #2 two, #1 one)
  end

fun all_products (xs : (int * int) list) =
  (* base case *)
  if null xs then []  
  else
      let
	  fun go_through_list (x,xs) =
	    (* x already exists, checked earlier *)
	    (* if x is the only argument left then:  *)
	    if null xs then one_pair_options x (* base case, returns a list *)
	    (* else go through hds (tl xs) *)				     
	    else t2_to_0 (x, hd xs) @ go_through_list (x, tl xs) (* shorten the list by one *)
      in
	   qsort (remove_duplicates (1 :: (go_through_list (hd xs, tl xs) @ all_products (tl xs))))
      end

(* 26 *)
(* === Positive Numbers === *)

(* Write a function 𝚒𝚜_𝚙𝚘𝚜𝚒𝚝𝚒𝚟𝚎 that takes an integer number and evaluates to 𝚝𝚛𝚞𝚎 or 𝚏𝚊𝚕𝚜𝚎. The function should evaluate to 𝚝𝚛𝚞𝚎 if its argument is a positive number, and to 𝚏𝚊𝚕𝚜𝚎 otherwise.

SIGNATURE: 𝚟𝚊𝚕 𝚒𝚜_𝚙𝚘𝚜𝚒𝚝𝚒𝚟𝚎 = 𝚏𝚗 : 𝚒𝚗𝚝 -> 𝚋𝚘𝚘𝚕
EXAMPLE: 𝚒𝚜_𝚙𝚘𝚜𝚒𝚝𝚒𝚟𝚎 𝟷 = 𝚝𝚛𝚞𝚎 *)
fun is_positive x =
  x > 0
	   
	    
(* 27 *)
(* === Divisibility === *)

(* Write a function 𝚒𝚜_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 that takes two integer numbers and evaluates to 𝚝𝚛𝚞𝚎 or 𝚏𝚊𝚕𝚜𝚎. It should evaluate to 𝚝𝚛𝚞𝚎 if its first argument is divisible by its second argument, and to 𝚏𝚊𝚕𝚜𝚎 otherwise. You may assume that the second argument will be non-zero.

SIGNATURE: 𝚟𝚊𝚕 𝚒𝚜_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 = 𝚏𝚗 : 𝚒𝚗𝚝 * 𝚒𝚗𝚝 -> 𝚋𝚘𝚘𝚕
EXAMPLE: 𝚒𝚜_𝚍𝚒𝚟𝚒𝚜𝚒𝚋𝚕𝚎_𝚋𝚢 (𝟼, 𝟹) = 𝚝𝚛𝚞𝚎 *)
fun is_divisible_by (x,y) =
  x mod y = 0
	   
(* 28 *)
(* === Integer Division === *)

(* Write a function 𝚍𝚒𝚟𝚒𝚍𝚎_𝚋𝚢 that takes two integer numbers and evaluates to the result of the integer division of the first one by the second one. You may assume that the first argument is non-negative and the second one is strictly positive. You should not use the 𝚍𝚒𝚟 operator.

HINT: Recall the 𝚙𝚘𝚠 function in the lectures.

SIGNATURE: 𝚟𝚊𝚕 𝚍𝚒𝚟𝚒𝚍𝚎_𝚋𝚢 = 𝚏𝚗 : 𝚒𝚗𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝
EXAMPLE: 𝚍𝚒𝚟𝚒𝚍𝚎_𝚋𝚢 (𝟽, 𝟹) = 𝟸 *)		
fun divide_by (x, y) =
  if x >= y then 1 + divide_by (x-y, y)
  else 0
	   
(* 29 *)
(* === Greatest Common Divisor === *)

(* Write a function 𝚐𝚌𝚍 that takes two integer numbers and evaluates to their greatest common divisor. Use the Euclidean algorithm:

http://en.wikipedia.org/wiki/Greatest_common_divisor

You may assume that both numbers are positive.

SIGNATURE: 𝚟𝚊𝚕 𝚐𝚌𝚍 = 𝚏𝚗 : 𝚒𝚗𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝
EXAMPLE: 𝚐𝚌𝚍 (𝟷𝟾, 𝟷𝟸) = 𝟼 *)
fun gcd (x,y) =
  let
      val xs = all_products (factorize x)
      val ys = all_products (factorize y)
      fun go_through_lists (first, second) =
	if null first then []
	else if null second then go_through_lists (tl first, ys)			
	else if hd first = hd second then (hd first) :: go_through_lists (tl first, ys)
	else go_through_lists (first, tl second)
      fun max xs =
	if null xs then 1
	else
	    let
		val max_xs = max (tl xs)
	    in
		if max_xs > hd xs then max_xs else hd xs
	    end		      
  in
      max (go_through_lists(xs,ys))
  end
      
(* 30 *)
(* === Least Common Multiple === *)

(* Write a function 𝚕𝚌𝚖 that takes two integer numbers and evaluates to their least common multiple. LCM can be defined in terms of GCD:

http://en.wikipedia.org/wiki/Least_common_multiple

You may assume that both numbers are positive.

SIGNATURE: 𝚟𝚊𝚕 𝚕𝚌𝚖 = 𝚏𝚗 : 𝚒𝚗𝚝 * 𝚒𝚗𝚝 -> 𝚒𝚗𝚝
EXAMPLE: 𝚕𝚌𝚖 (𝟷𝟾, 𝟷𝟸) = 𝟹𝟼 *)
fun lcm (x,y) =
  (x * y) div gcd(x,y)
  
