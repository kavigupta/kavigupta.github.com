public class Star {
    public double x, y, brightness;
    public Star(double x, double y, double brightness) {
        this.x = x;
        this.y = y;
        this.brightness = brightness;
    }
    @Override
    public String toString() {
        return "Star [" + x + ", " + y + ", " + brightness + "]";
    }
}