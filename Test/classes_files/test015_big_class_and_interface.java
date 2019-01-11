@annotation(a=1, b=true, c='e')
public abstract strictfp interface myInterface<firstInterface, secondInterface> extends fourthInterface, fithInterface{}

@annotation(42)
private final class myClass<firstClass, secondClass> extends thirdClass<fourthClass> implements myInterface<firstInterface, secondInterface>, thirdInterface {}
