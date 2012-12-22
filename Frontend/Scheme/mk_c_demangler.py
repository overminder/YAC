from mangler_def import demangle_table

func_name = 'yac_demangle_symbol'

tmpl = '''
#include <string.h>
#include <stdlib.h>

static int yac_demangle_table[] = {
    %(table)s
};

char *
%(func)s(const char *mangled) {
    int i, j;
    int len;
    char *demangled;

    if (!mangled) {
        return 0;
    }

    i = 0, j = 0;
    len = strlen(mangled);
    demangled = malloc(len + 1);
    while (i < len) {
        char c = mangled[i];
        if (c == 'z') {
            demangled[j++] = yac_demangle_table[mangled[i + 1]];
            i += 2;
        }
        else {
            demangled[j++] = c;
            i += 1;
        }
    }
    demangled[j] = '\\0';
    return demangled;
}
'''

print tmpl % {
    'func': func_name,
    'table': ',\n    '.join(str(to_i) for to_i in demangle_table)
}

