module KnuthBendix = struct
  
  type relations = (string * string) list

  let print_r r =
    let aux (u,v) =
      Printf.printf "%s %s ; " u v; in
    List.iter aux r;;
    
  let included s1 s2 =
    let l1 = String.length s1 and l2 = String.length s2 in
    let j = ref (-1) in
    for i=0 to l2-l1 do
      if ((String.sub s2 i l1) = s1) then j := i;
    done;
    !j
    
  let normal_form word (r : relations) = 
    let rec aux word (r : relations) = function
      |(s1, s2)::q ->
        let j = included s1 word in
        if j = -1 then aux word r q
        else(
          let new_word = (String.sub word 0 j)^s2^(String.sub word (j + (String.length s1)) ((String.length word) - j - (String.length s1))) in
          aux new_word r r
        )
      |[] -> word in
    aux word r r
           
  let reduce_by u v e r =
    let rec aux u v new_r to_add = function
      |(ui, vi)::q -> let ui' = (normal_form ui [(u, v)]) in
                      if (ui' != ui) then (aux u v new_r ((ui', vi)::to_add) q)
                      else (aux u v ((ui, vi)::new_r) to_add q);
      |[] -> (to_add, new_r) in
    (aux u v [] e r)

  let normalize_v (r : relations) =
    let rec aux r = function
      |(u, v)::q -> (u, (normal_form v r))::(aux r q)
      |[] -> [] in
    aux r r

  let critical_pairs (r : relations) = 
    let rec aux (u,v) = function
      |(ui, vi)::q ->
        let to_add = ref (aux (u,v) q) in
        let u1 = ref ui and u2 = ref u and v1 = ref vi and v2 = ref v in
        if (String.length u < String.length ui)
        then (u1 := u;
              u2 := ui;
              v1 := v;
              v2 := vi;);
        let l1 = String.length !u1 and l2 = String.length !u2 in
        for i = 1 to (l1 - 1) do
          let m1 = String.sub !u1 (l1 - i) i in
          let m2 = String.sub !u2 0 i in
          if (m1 = m2) then
            to_add := (!v1^(String.sub !u2 i (l2-i)), (String.sub !u1 0 (l1 - i))^(!v2))::(!to_add);
        done;
        for i = 0 to (l2 - l1) do
          let m2 = String.sub !u2 i l1 in
          if (!u1 = m2) then
            to_add := (!v2, (String.sub !u2 0 i)^(!v1)^(String.sub !u2 (i+l1) (l2 - l1 -i)))::(!to_add);
        done;
        for i = 1 to (l1 - 1) do
          let m1 = String.sub !u1 0 (l1 - i) in
          let m2 = String.sub !u2 (l2-l1+i) (l1-i) in
          if (m1 = m2) then
            to_add := ((String.sub !u2 0 (l2-l1+i))^(!v1), !v2^(String.sub !u1 (l1-i) i))::(!to_add);
        done;
        !to_add
      |[] -> [] in
    aux (List.hd r) (List.tl r)
                      
  let procedure (r : relations) =
    let rec aux (r : relations) (e : relations) =
      Printf.printf "\n\nR :";
      print_r r;
      Printf.printf "\nE :";
      print_r e;
      match e with
      |(u, v)::q ->
        let u1 = (normal_form u r) and v1 = (normal_form v r) in
        if (u1 = v1) then (aux r q)
        else (
          let u2 = ref u1 and v2 = ref v1 in
          if (u1 < v1) then (u2 := v1;
                             v2 := u1;);
          let new_E, new_r = (reduce_by !u2 !v2 q r) in
          let q2 = normalize_v ((!u2, !v2)::new_r) in
          
          (aux q2 (new_E@(critical_pairs q2)))
        )
      |[] -> r in
    aux [] r

  let sub_procedure (r : relations) =
    let rec aux (r : relations) (e : relations) =
      Printf.printf "\n\nR :";
      print_r r;
      Printf.printf "\nE :";
      print_r e;
      match e with
      |(u, v)::q ->
        if (u = v) then (aux r q)
        else (
          let u1 = ref u and v1 = ref v in
          if (u < v) then (u1 := v;
                             v1 := u;);
          let v2 = (normal_form !v1 r) in
          let new_E, new_r = (reduce_by !u1 v2 q r) in
          let q2 = normalize_v ((!u1, v2)::new_r) in
          
          (aux q2 (new_E@(critical_pairs q2)))
        )
      |[] -> r in
    aux [] r
end

let ex1 = KnuthBendix.sub_procedure [("aaaaaa", ""); ("aa", "A")]
let ex2 = KnuthBendix.sub_procedure [("ab", "A"); ("aa", ""); ("bb",""); ("bab","aba")]
let pres = KnuthBendix.sub_procedure [("ab", "A"); ("ac", "B"); ("aa", ""); ("bb",""); ("cc",""); ("bab","aba"); ("cbc","bcb"); ("ca","ac"); ("cbac", "bcba")]
