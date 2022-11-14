module Twelf =
    struct
        type tt = Type of string
        type const = Constant of string * tt
        type func = Function of string * tt list * tt
        type rule = Rule of string * parameter
        and parameter = ParamC of const | ParamF of func * parameter list | ParamV of variable
        and variable = Variable of string
        type context = Context of tt list * const list * func list * rule list

        exception AlreadyDefined
        let empty_context = Context([], [], [], [])

        let is_already_defined name context =
            let Context (types, constants, functions, rules) = context in
            if List.exists (fun (Type x) -> x = name) types
            && List.exists (fun (Constant (x, _)) -> x = name) constants
            && List.exists (fun (Function (x, _, _)) -> x = name) functions
            && List.exists (fun (Rule (x, _)) -> x = name) rules then
                raise AlreadyDefined
            else
                (types, constants, functions, rules)

        let get_function name (Context (_, _, functions, _)) =
            let ffs = List.filter (fun (Function (n, _, _)) -> name = n) functions in
            match ffs with
            | [] -> None
            | x :: xs -> Some x

        let define_type name context =
            let types, constants, functions, rules = is_already_defined name context in
            Context ((Type name) :: types, constants, functions, rules)

        let define_constant name ttype context =
            let types, constants, functions, rules = is_already_defined  name context in
            Context (types, (Constant (name, ttype)) :: constants, functions, rules)

        let define_function name types return_type context =
            let types, constants, functions, rules = is_already_defined name context in
            Context (types, constants, (Function (name, types, return_type)) :: functions, rules)

        let define_rule name parameter context =
            let types, constants, functions, rules = is_already_defined name context in
            Context (types, constants, functions, (Rule (name, parameter)) :: rules)

        (* TODO *)
        let rec eval func params context =
            let Context (types, constants, functions, rules) = context in
            let Function (fname, para_type, return_type) = func in
            let variables = List.filter (fun x -> match x with ParamV _ -> true | _ -> false) params
            |> List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] in
            variables
    end
;;
