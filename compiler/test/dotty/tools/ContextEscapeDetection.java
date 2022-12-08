package dotty.tools;

import dotty.tools.dotc.core.Contexts;
import org.junit.*;

import java.lang.ref.WeakReference;
import java.util.LinkedList;
import java.util.List;

public abstract class ContextEscapeDetection {
    public static class TestContext{
        public TestContext(WeakReference<Contexts.ContextCls> context, String testName) {
            this.context = context;
            this.testName = testName;
        }

        public final WeakReference<Contexts.ContextCls> context;
        public final String testName;

    }
    public static final List<TestContext> contexts = new LinkedList<TestContext>();

    public abstract Contexts.ContextCls getCtx();

    public abstract void clearCtx();

    @Before
    public synchronized void stealContext() {
        contexts.add(new TestContext(new WeakReference<Contexts.ContextCls>(this.getCtx()), this.getClass().getName()));
    }

    @After
    public synchronized void clearContext() {
        this.clearCtx();
    }
}
