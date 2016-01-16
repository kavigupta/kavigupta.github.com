import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.HashSet;
import java.util.Stack;
import java.util.function.Predicate;
import java.util.stream.IntStream;

public class Mathematics {
    public static Iterable<Coordinate<Integer>> cartRange(int xmin, int xmax,
            int ymin, int ymax, Predicate<Coordinate<Integer>> filter) {
        return IntStream.range(xmin, xmax).boxed()
                .flatMap(x -> IntStream.range(ymin, ymax)
                        .mapToObj(y -> new Coordinate<>(x, y)))
                .filter(filter)::iterator;
    }
    public static Iterable<Coordinate<Integer>> cartSquare(int x0, int y0,
            int radius, Predicate<Coordinate<Integer>> filter) {
        return cartRange(x0 - radius, x0 + radius, y0 - radius, y0 + radius,
                filter);
    }
    public static HashSet<Coordinate<Integer>> populateCluster(float[][] greys,
            Coordinate<Integer> start, Predicate<Float> filter) {
        Stack<Coordinate<Integer>> toprocess = new Stack<>();
        HashSet<Coordinate<Integer>> included = new HashSet<>();
        toprocess.add(start);
        while (!toprocess.isEmpty()) {
            Coordinate<Integer> xy = toprocess.pop();
            int x = xy.x, y = xy.y;
            if (x < 0 || y < 0 || x >= greys.length || y >= greys[0].length)
                continue;
            if (!filter.test(greys[x][y])) continue;
            if (included.contains(new Coordinate<>(x, y))) continue;
            included.add(new Coordinate<>(x, y));
            toprocess.push(new Coordinate<>(x + 1, y));
            toprocess.push(new Coordinate<>(x, y + 1));
            toprocess.push(new Coordinate<>(x - 1, y));
            toprocess.push(new Coordinate<>(x, y - 1));
        }
        return included;
    }
    public static float[][] getGreys(BufferedImage bi) {
        int width = bi.getWidth();
        int height = bi.getHeight();
        float[][] greys = new float[width][height];
        for (Coordinate<Integer> xy : Mathematics.cartRange(0, width, 0, height,
                u -> true)) {
            Color c = new Color(bi.getRGB(xy.x, xy.y));
            float[] hsb = new float[3];
            Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), hsb);
            greys[xy.x][xy.y] = hsb[2];
        }
        return greys;
    }
}
