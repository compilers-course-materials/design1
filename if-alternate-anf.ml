open Printf

type reg =
  | EAX
  | ESP
  | EBP


type size =
  | DWORD_PTR
  | WORD_PTR
  | BYTE_PTR

type arg =
  | Const of int
  | HexConst of int
  | Reg of reg
  | RegOffset of int * reg
  | Sized of size * arg

type instruction =
  | IMov of arg * arg

  | IAdd of arg * arg
  | ISub of arg * arg
  | IMul of arg * arg

  | IShr of arg * arg
  | ISar of arg * arg
  | IShl of arg * arg

  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg

  | ILabel of string
  | IPush of arg
  | IPop of arg
  | ICall of string
  | IRet

  | ICmp of arg * arg
  | IJne of string
  | IJe of string
  | IJmp of string
  | IJno of string
  | IJo of string


type prim1 =
  | Add1
  | Sub1
  | Print
  | IsNum
  | IsBool

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal

type expr =
  | ELet of (string * expr) list * expr
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | EIf of expr * expr * expr
  | ENumber of int
  | EBool of bool
  | EId of string


type immexpr =
  | ImmNumber of int
  | ImmBool of bool
  | ImmId of string

and cexpr =
  | CPrim1 of prim1 * immexpr
  | CPrim2 of prim2 * immexpr * immexpr
  | CImmExpr of immexpr

type aexpr =
  | AIf of immexpr * aexpr * aexpr
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr


let count = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let rec anf e (k : immexpr -> aexpr) =
  match e with
    | ELet([], body) -> anf body k
    | ELet((name, value)::rest, body) ->
      anf value (fun imm ->
        ALet(name, CImmExpr(imm),
          anf (ELet(rest, body)) k))
    | EPrim1(op, e) ->
      let tmp = gen_temp "prim1" in
      anf e (fun imm ->
        ALet(tmp, CPrim1(op, imm), k(ImmId(tmp))))
    | EPrim2(op, left, right) ->
      let tmp = gen_temp "prim2" in
      anf left (fun immleft ->
        anf right (fun immright ->
          ALet(tmp, CPrim2(op, immleft, immright), k(ImmId(tmp)))))
    | EIf(cond, thn, els) ->
      anf cond (fun immcond -> AIf(immcond, anf thn k, anf els k))
    | ENumber(n) -> k(ImmNumber(n))
    | EBool(n) -> k(ImmBool(n))
    | EId(name) -> k(ImmId(name))


let r_to_asm (r : reg) : string =
  match r with
    | EAX -> "eax"
    | ESP -> "esp"
    | EBP -> "ebp"

let s_to_asm (s : size) : string =
  match s with
    | DWORD_PTR -> "DWORD"
    | WORD_PTR -> "WORD"
    | BYTE_PTR -> "BYTE"

let rec arg_to_asm (a : arg) : string =
  match a with
    | Const(n) -> sprintf "%d" n
    | HexConst(n) -> sprintf "0x%X" n
    | Reg(r) -> r_to_asm r
    | RegOffset(n, r) ->
      if n >= 0 then
        sprintf "[%s+%d]" (r_to_asm r) n
      else
        sprintf "[%s-%d]" (r_to_asm r) (-1 * n)
    | Sized(s, a) ->
      sprintf "%s %s" (s_to_asm s) (arg_to_asm a)

let i_to_asm (i : instruction) : string =
  match i with
    | IMov(dest, value) ->
      sprintf "  mov %s, %s" (arg_to_asm dest) (arg_to_asm value)
    | IAdd(dest, to_add) ->
      sprintf "  add %s, %s" (arg_to_asm dest) (arg_to_asm to_add)
    | ISub(dest, to_sub) ->
      sprintf "  sub %s, %s" (arg_to_asm dest) (arg_to_asm to_sub)
    | IMul(dest, to_mul) ->
      sprintf "  imul %s, %s" (arg_to_asm dest) (arg_to_asm to_mul)
    | IAnd(dest, mask) ->
      sprintf "  and %s, %s" (arg_to_asm dest) (arg_to_asm mask)
    | IOr(dest, mask) ->
      sprintf "  or %s, %s" (arg_to_asm dest) (arg_to_asm mask)
    | IXor(dest, mask) ->
      sprintf "  xor %s, %s" (arg_to_asm dest) (arg_to_asm mask)
    | IShr(dest, to_shift) ->
      sprintf "  shr %s, %s" (arg_to_asm dest) (arg_to_asm to_shift)
    | ISar(dest, to_shift) ->
      sprintf "  sar %s, %s" (arg_to_asm dest) (arg_to_asm to_shift)
    | IShl(dest, to_shift) ->
      sprintf "  shl %s, %s" (arg_to_asm dest) (arg_to_asm to_shift)
    | ICmp(left, right) ->
      sprintf "  cmp %s, %s" (arg_to_asm left) (arg_to_asm right)
    | IPush(arg) ->
      sprintf "  push %s" (arg_to_asm arg)
    | IPop(arg) ->
      sprintf "  pop %s" (arg_to_asm arg)
    | ICall(str) ->
      sprintf "  call %s" str
    | ILabel(name) ->
      sprintf "%s:" name
    | IJne(label) ->
      sprintf "  jne near %s" label
    | IJe(label) ->
      sprintf "  je near %s" label
    | IJno(label) ->
      sprintf "  jno near %s" label
    | IJo(label) ->
      sprintf "  jo near %s" label
    | IJmp(label) ->
      sprintf "  jmp near %s" label
    | IRet ->
      "  ret"

let to_asm (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (i_to_asm i)) "" is

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let const_true = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let acompile_imm_arg (i : immexpr) _ (env : (string * int) list) : arg =
  match i with
    | ImmNumber(n) ->
      if n > 1073741823 || n < -1073741824 then
        failwith ("Integer overflow: " ^ (string_of_int n))
      else
        Const((n lsl 1))
    | ImmBool(b) ->
      if b then const_true else const_false
    | ImmId(name) ->
      begin match find env name with
        | Some(stackloc) -> RegOffset(-4 * stackloc, EBP)
        | None -> failwith ("Unbound identifier" ^ name)
      end

let acompile_imm (i : immexpr) (si : int) (env : (string * int) list) : instruction list =
  [ IMov(Reg(EAX), acompile_imm_arg i si env) ]

let throw_err code = 
  [
    IPush(Sized(DWORD_PTR, Const(code)));
    ICall("error");
  ]

let check_overflow = IJo("overflow_check")
let error_non_int = "error_non_int"
let error_non_bool = "error_non_bool"

let check_num =
  [
    IAnd(Reg(EAX), Const(0x00000001));
    ICmp(Reg(EAX), Const(0x00000000));
  ]

let max n m = if n > m then n else m
let rec count_c_vars (ce : cexpr) : int =
  match ce with
    | _ -> 0

and count_vars (ae : aexpr) : int =
  match ae with
    | ALet(x, bind, body) -> 
      1 + (max (count_c_vars bind) (count_vars body))
    | AIf(_, thn, els) ->
      max (count_vars thn) (count_vars els)
    | ACExpr(ce) -> count_c_vars ce

let check_nums arg1 arg2 =
  [
    IMov(Reg(EAX), arg1) 
  ] @ check_num @ [
    IJne(error_non_int);
    IMov(Reg(EAX), arg2);
  ] @ check_num @ [
    IJne(error_non_int);
  ]

let rec acompile_step (s : cexpr) (si : int) (env : (string * int) list) : instruction list =
  match s with
    | CPrim1(op, e) ->
      let prelude = acompile_imm e si env in
      begin match op with
        | Add1 ->
          prelude @ [
            IAdd(Reg(EAX), Const(2))
          ]
        | Sub1 ->
          prelude @ [
            IAdd(Reg(EAX), Const(-2))
          ]
        | IsNum ->
          prelude @ [
            IAnd(Reg(EAX), Const(0x00000001));
            IShl(Reg(EAX), Const(31));
            IXor(Reg(EAX), Const(0xFFFFFFFF));
          ]
        | IsBool ->
          prelude @ [
            IAnd(Reg(EAX), Const(0x00000001));
            IShl(Reg(EAX), Const(31));
            IOr(Reg(EAX), Const(0x7FFFFFFF));
          ]
        | Print ->
          prelude @ [
(*            ISub(Reg(ESP), Const(si * 4)); *)
(*            ISub(Reg(ESP), Sized(DWORD_PTR, Const(8))); *)
            IPush(Sized(DWORD_PTR, Reg(EAX))); (*
            IMov(Reg(EBP), Reg(ESP)); *)
            ICall("print");
            IPop(Reg(EAX));
          ]
      end

    | CPrim2(op, left, right) ->
      let left_as_arg = acompile_imm_arg left si env in
      let right_as_arg = acompile_imm_arg right si env in
      let checked = check_nums left_as_arg right_as_arg in
      begin match op with
        | Plus ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            IAdd(Reg(EAX), right_as_arg);
            check_overflow
          ]
        | Minus ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISub(Reg(EAX), right_as_arg);
            check_overflow
          ]
        | Times ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISar(Reg(EAX), Const(1));
            IMul(Reg(EAX), right_as_arg);
            check_overflow;
          ]
        | Less ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISub(Reg(EAX), right_as_arg);
            ISub(Reg(EAX), Const(1));
            IAnd(Reg(EAX), HexConst(0x80000000));
            IOr( Reg(EAX), HexConst(0x7FFFFFFF));
          ]
        | Greater ->
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ISub(Reg(EAX), right_as_arg);
            IAnd(Reg(EAX), HexConst(0x80000000));
            IXor(Reg(EAX), HexConst(0xFFFFFFFF));
          ]
        | Equal ->
          let leave_false = gen_temp "equals" in
          checked @
          [
            IMov(Reg(EAX), left_as_arg);
            ICmp(Reg(EAX), right_as_arg);
            IMov(Reg(EAX), const_false);
            IJne(leave_false);
            IMov(Reg(EAX), const_true);
            ILabel(leave_false);
          ]
       end
    | CImmExpr(i) -> acompile_imm i si env

and acompile_expr (e : aexpr) (si : int) (env : (string * int) list) : instruction list =
  match e with
    | ALet(id, e, body) ->
      let prelude = acompile_step e (si + 1) env in
      let postlude = acompile_expr body (si + 1) ((id, si)::env) in
      prelude @ [
        IMov(RegOffset(-4 * si, EBP), Reg(EAX))
      ] @ postlude
    | AIf(cond, thn, els) ->
      let prelude = acompile_imm cond si env in
      let thn = acompile_expr thn si env in
      let els = acompile_expr els si env in
      let label_then = gen_temp "then" in
      let label_else = gen_temp "else" in
      let label_end = gen_temp "end" in
      prelude @ [
        ICmp(Reg(EAX), const_true);
        IJe(label_then);
        ICmp(Reg(EAX), const_false);
        IJe(label_else);
        IJmp(error_non_bool);
        ILabel(label_then)
      ] @
      thn @
      [ IJmp(label_end); ILabel(label_else) ] @
      els @
      [ ILabel(label_end) ]
    | ACExpr(s) -> acompile_step s si env

let compile_to_string prog =
  let anfed = (anf prog (fun i -> ACExpr(CImmExpr(i)))) in
  let varcount = count_vars anfed in
  let stackjump = 4 * varcount in
  let prelude = "section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, " ^ (string_of_int stackjump) ^ "\n" in
  let postlude = [
    IMov(Reg(ESP), Reg(EBP));
    IPop(Reg(EBP));
    IRet;
    ILabel("overflow_check")
  ]
  @ (throw_err 3)
  @ [ILabel(error_non_int)] @ (throw_err 1)
  @ [ILabel(error_non_bool)] @ (throw_err 2) in
  let compiled = (acompile_expr anfed 1 []) in
  let as_assembly_string = (to_asm (compiled @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string

