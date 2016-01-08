import java.awt.image.BufferedImage;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;

public class StarIdentification {
    public static final int[] VISUALIZATION_COLORS = { 0xFF0000, 0xFF8800,
            0xFFFF00, 0x00FF00, 0x00FFFF, 0x0000FF, 0xFF00FF };
    /**
     * Maps coordinates to the intensity of the star centered at that coordinate
     */
    private HashMap<Coordinate<Integer>, Star> centers = new HashMap<>();
    private int width, height;
    public BufferedImage coloredStars;
    public StarIdentification(BufferedImage bi, float threshold) {
        width = bi.getWidth();
        height = bi.getHeight();
        coloredStars = new BufferedImage(width, height,
                BufferedImage.TYPE_INT_RGB);
        float[][] greys = Mathematics.getGreys(bi);
        System.out.println("Greyed");
        int ncluster = 0;
        for (Coordinate<Integer> xy : Mathematics.cartRange(0, width, 0, height,
                xy -> coloredStars.getRGB(xy.x, xy.y) != 0)) {
            HashSet<Coordinate<Integer>> current = Mathematics
                    .populateCluster(greys, xy, level -> level > threshold);
            int color = VISUALIZATION_COLORS[(ncluster++)
                    % VISUALIZATION_COLORS.length];
            for (Coordinate<Integer> u : current)
                coloredStars.setRGB(u.x, u.y, color);
            if (current.size() == 0) continue;
            int xavg = 0, yavg = 0;
            double brightness = 0;
            for (Coordinate<Integer> c : current) {
                double cbright = greys[c.x][c.y];
                xavg += c.x * cbright;
                yavg += c.y * cbright;
                brightness += cbright;
            }
            xavg /= brightness;
            yavg /= brightness;
            centers.put(new Coordinate<>(xavg, yavg),
                    new Star(xavg, yavg, brightness));
        }
        System.out.println("Identified centers");
    }
    public List<Entry<Coordinate<Integer>, Star>> brightestStars(int number) {
        List<Entry<Coordinate<Integer>, Star>> all = new ArrayList<>(
                centers.entrySet());
        all.sort((x, y) -> -Double.compare(x.getValue().brightness,
                y.getValue().brightness));
        return all.subList(0, Math.min(number, all.size()));
    }
    public List<Coordinate<Star>> associate(StarIdentification other,
            int distance, double diff) {
        // get top 25% of stars
        List<Entry<Coordinate<Integer>, Star>> brightest = brightestStars(
                this.centers.size() / 4);
        // compile list of possible offsets from brightest star
        Star brightestStar = brightest.get(0).getValue();
        if (brightestStar == null) { throw new IllegalStateException(); }
        HashMap<Coordinate<Integer>, Integer> offs = new HashMap<>();
        int x0 = brightest.get(0).getKey().x, y0 = brightest.get(0).getKey().y;
        for (Coordinate<Integer> xy : Mathematics.cartSquare(x0, y0, distance,
                other.similarBrightnessTo(diff, brightestStar))) {
            offs.put(new Coordinate<>(xy.x - x0, xy.y - y0), 0);
        }
        if (offs.size() == 0) throw new IllegalStateException(
                "No decent offsets found. Please loosen restrictions");
        brightest.remove(0);
        System.out.println("Offsets\n" + offs);
        // give anything that makes sense a point.
        for (Entry<Coordinate<Integer>, Star> s : brightest) {
            for (Coordinate<Integer> off : offs.keySet()) {
                x0 = off.x + s.getKey().x;
                y0 = off.y + s.getKey().y;
                // if there is star in the other set with a similar brightness
                // to this one within distance/10, the offset gets a point
                if (Mathematics
                        .cartSquare(x0, y0, distance / 5,
                                other.similarBrightnessTo(diff, s.getValue()))
                        .iterator().hasNext()) {
                    offs.put(off, offs.get(off) + 1);
                }
            }
        }
        // find the offset that makes the most sense
        ArrayList<Entry<Coordinate<Integer>, Integer>> bestOffsets = new ArrayList<>(
                offs.entrySet());
        Coordinate<Integer> bestOff = bestOffsets.stream()
                .max(Comparator.comparing(Entry::getValue)).get().getKey();
        ArrayList<Coordinate<Star>> stars = new ArrayList<>();
        for (Entry<Coordinate<Integer>, Star> s : centers.entrySet()) {
            x0 = bestOff.x + s.getKey().x;
            y0 = bestOff.y + s.getKey().y;
            Iterator<Coordinate<Integer>> similarToThis = Mathematics
                    .cartSquare(x0, y0, distance / 5,
                            other.similarBrightnessTo(diff, s.getValue()))
                    .iterator();
            if (similarToThis.hasNext())
                stars.add(new Coordinate<>(s.getValue(),
                        other.centers.get(similarToThis.next())));
        }
        System.out.printf("Corresponding stars found for %.2f%% of stars\n",
                100. * stars.size() / centers.size());
        return stars;
    }
    public Predicate<Coordinate<Integer>> similarBrightnessTo(double diff,
            Star compareTo) {
        return xy -> {
            if (!this.centers.containsKey(xy)) return false;
            return Math
                    .abs(this.centers.get(xy).brightness - compareTo.brightness)
                    / compareTo.brightness < diff;
        };
    }
    public boolean contained(Coordinate<Integer> coor) {
        return coor.x >= 0 && coor.x < width && coor.y >= 0 && coor.y < height;
    }
    @Override
    public String toString() {
        return "StarIdentification [centers=" + centers + "]";
    }
}
