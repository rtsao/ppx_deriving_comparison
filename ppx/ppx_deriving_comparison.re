module P = Ppxlib;
module A = P.Ast_builder.Default;
module D = Ppxlib.Deriving;

let derive_comparison =
    (
      ~loc: P.location,
      ~path as _,
      (_: P.rec_flag, tdls: list(P.type_declaration)),
    )
    : P.structure => {
  open P;
  let from_type_decl = (td: P.type_declaration) => {
    let patterns =
      switch (td.ptype_kind) {
      | Ptype_variant(cdl) =>
        List.map(
          cd => {
            let arity =
              switch (cd.pcd_args) {
              | Pcstr_tuple(list) => List.length(list)
              | Pcstr_record(list) => List.length(list)
              };

            let args =
              switch (arity) {
              | 0 => None
              | _ => Some(A.ppat_any(~loc))
              };

            let pat =
              A.ppat_construct(
                ~loc,
                {txt: Longident.Lident(cd.pcd_name.txt), loc},
                args,
              );
            A.ppat_tuple(~loc, [pat, pat]);
          },
          cdl,
        )
      | _ => failwith("ppx_deriving_comparison expects a variant")
      };

    let union =
      List.fold_left(
        (acc, el) => A.ppat_or(~loc, acc, el),
        List.hd(patterns),
        List.tl(patterns),
      );

    let name = Printf.sprintf("%s_is_same_constructor", td.ptype_name.txt);
    let binding = A.pvar(~loc, name);
    let btype = A.ptyp_any(~loc);
    let pat = A.ppat_constraint(~loc, binding, btype);
    let expr = [%expr
      (a, b) =>
        switch (a, b) {
        | [%p union] => true
        | _ => false
        }
    ];

    let vb = A.value_binding(~loc, ~pat, ~expr);
    A.pstr_value(~loc, P.Nonrecursive, [vb]);
  };

  List.map(from_type_decl, tdls);
};

let () = {
  let str_type_decl = D.Generator.make_noarg(derive_comparison);
  D.ignore(D.add("comparison", ~str_type_decl));
};
