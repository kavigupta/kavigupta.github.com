public interface MRTFunction<In, L, R> {
    public R apply(In input) throws Left<L>;
}