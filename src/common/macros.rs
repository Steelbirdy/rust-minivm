macro_rules! visit_trait {
    [$instr:ty: $($variant:ident => $func:ident($($($([$borrow:tt])? $arg:ident: $arg_ty:ty),+)?);)+] => {
        #[allow(unused)]
        pub trait Visit {
            fn visit(&mut self, instr: &$instr) {
                use $instr::*;

                match instr {
                    $(
                    $variant $({ $($arg),+ })? => self.$func($($(visit_trait!(@deref $arg $($borrow)?)),+)?),
                    )+
                }
            }

            $(
            fn $func(&mut self $($(, $arg: $arg_ty)+)?) {}
            )+
        }
    };
    (@deref $arg:ident &) => {
        $arg
    };
    (@deref $arg:ident) => {
        *$arg
    };
}
