

#[macro_export(crate)]
macro_rules! tuple_macro {
    ($m:ident) => {
        tuple_macro!(
            @rec
            $m
            (T0, T1, T2, T3, T4, T5, T6, T7,)
        );
    };
    (@rec $m:ident ()) => {
        $m!();
    };
    (@rec $m:ident ($x:ident, $($xs:ident,)*)) => {
        $m!($x, $($xs,)*);
        tuple_macro!(@rec $m ($($xs,)*));
    };
}
