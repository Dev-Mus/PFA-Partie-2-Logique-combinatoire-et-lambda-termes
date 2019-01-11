(** S, K, I appelées combinateurs de base. **)
type base = 
	S | K | I 
;;

(** ensemble des variables **)
type variable = 
	V of string 
;;

(** cl_term **)
type cl_term = 
	Base of base 
	|Var of variable (* K, S, I et chaque variable est un CL-terme *)
	|App of cl_term*cl_term (* l'application d'un CL-terme à un CL-terme donne un CL-terme. *)
;; 


(** DEF lambda_Term type generique **)
type ('a,'b) lambda_Term = 
	LVar of 'b 
	| LAbs of 'a * ('a,'b) lambda_Term 
	| LApp of ('a,'b) lambda_Term * ('a,'b) lambda_Term
;;

(** algo d’abstraction **)

let rec abstraction = function
	|LVar x ->  Var (V x)
	|LApp (x, y) -> App((abstraction x), (abstraction y))	
	|LAbs (x, y) ->
					match y with 
					|LVar x1 -> if x=x1 then Base I  (* [x]x =def I *)
								else App (Base K, Var (V x1)) (* [x]y =def Ky (x≠y) *)
					|LApp(m,n) -> App(App(Base S, abstraction(LAbs(x,m))), abstraction(LAbs(x,n))) (* [x](MN) =def S ([x]M) ([x]N) *)
					|x1 -> App (Base K, abstraction x1) (* [x]y =def Ky (x≠y) *)
;;

(*
@param1 : lambda_Term
@return : cl_term
 val abstraction : (string, string) lambda_Term -> cl_term = <fun>
*)

(** DEF lambda_Term var of string **)
type lambda_Term = 
	LVar of string 
	| LAbs of string * lambda_Term 
	| LApp of lambda_Term * lambda_Term
;;



