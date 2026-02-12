package example;

public class Issue4505J {

    public @interface AnnotationWithEnum {
        AnnotationEnum value();
    }

    public @interface AnnotationWithClass {
        Class<?> value();
    }

    public enum AnnotationEnum {
        Val1
    }

}
