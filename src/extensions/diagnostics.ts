import { linter, Diagnostic } from "@codemirror/lint";
import { syntaxTree } from "@codemirror/language"
import { EditorView } from "@codemirror/view"

function lintExample(view: EditorView): readonly Diagnostic[] {
    const diagnostics: Diagnostic[] = [];

    syntaxTree(view.state).iterate({
        enter: (type, from, to) => {
            if (type.isError) {
                diagnostics.push({
                    from,
                    to,
                    severity: "error",
                    message: "Parsing error!",
                });
            }
        },
    });

    return diagnostics;
}

export const parseErrors = linter(lintExample)