module Twelf =
    struct
        type tt = Type of string
        type const = Constant of string * tt
        type func = Function of string * tt list * tt
        type rule = Rule of string * func list
        type context = Context of tt list * const list * func list * rule list
        type variable = Variable of string
        type parameter = ParamC of const | ParamF of func * parameter list | ParamV of variable

        exception AlreadyDefined

        let is_already_defined name context =
            let Context (types, constants, functions, rules) = context in
            if List.exists (fun (Type x) -> x = name) types
            && List.exists (fun (Constant (x, _)) -> x = name) constants
            && List.exists (fun (Function (x, _, _)) -> x = name) functions
            && List.exists (fun (Rule (x, _)) -> x = name) rules then
                raise AlreadyDefined
            else
                (types, constants, functions, rules)

        let define_type name context =
            let types, constants, functions, rules = is_already_defined name context in
            Context ((Type name) :: types, constants, functions, rules)

        let define_constant name ttype context =
            let types, constants, functions, rules = is_already_defined  name context in
            Context (types, (Constant (name, ttype)) :: constants, functions, rules)

        let define_function name types return_type context =
            let types, constants, functions, rules = is_already_defined name context in
            Context (types, constants, (Function (name, types, return_type)) :: functions, rules)

        let define_rule name functions context =
            let types, constants, functions, rules = is_already_defined name context in
            Context (types, constants, functions, (Rule (name, functions)) :: rules)

        (* TODO *)
        let rec eval func params context =
            let Context (types, constants, functions, rules) = context in
            let Function (fname, para_type, return_type) = func in
            let variables = List.filter (fun x -> match x with ParamV _ -> true | _ -> false) params in
            let func_rules = List.filter (
                fun (Rule (_, ff)) ->
                    let (Function (ffn, _, _)) = List.nth ff 0 in
                    ffn = fname
            ) rules in
            func_rules
    end
;;
