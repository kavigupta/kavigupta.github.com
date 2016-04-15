public class Left<T> extends RuntimeException {
    public final T value;
    public Left(T value){
        this.value = value;
    }
}