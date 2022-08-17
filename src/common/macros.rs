macro_rules! visit_trait {
    [$instr:ty: $($variant:ident => $func:ident($($($([$borrow:tt])? $arg:ident: $arg_ty:ty),+)?);)+] => {
        #[allow(unused)]
        pub trait Visit {
            type Output;

            fn unimplemented(&mut self) -> Self::Output;

            fn visit(&mut self, instr: &$instr) -> Self::Output {
                use $instr::*;

                match instr {
                    $(
                    $variant $({ $($arg),+ })? => self.$func($($(visit_trait!(@deref $arg $($borrow)?)),+)?),
                    )+
                }
            }

            $(
            #[inline]
            fn $func(&mut self $($(, $arg: $arg_ty)+)?) -> Self::Output {
                self.unimplemented()
            }
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
