import os
import subprocess
from datetime import datetime

import matplotlib as mpl
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

mpl.rcParams['figure.dpi'] = 125
mpl.rcParams['hatch.linewidth'] = 4.0

root_process = subprocess.run(
    ['git', 'rev-parse', '--show-toplevel'], stdout=subprocess.PIPE
)
ROOT = root_process.stdout.decode('utf-8')[:-1]
DATA_ROOT = os.path.join(ROOT, '_data')
PICS_ROOT = os.path.join(ROOT, 'static', 'stats')

plt.style.use(os.path.join(ROOT, 'scripts', 'palenight.mplstyle'))


def remove_zeros(data):
    last = -1
    result = []
    for datum in data:
        if last <= 0 and datum > 0:
            if len(result) > 0:
                result[-1] = 0
            result.append(datum)
            last = datum
        elif last <= 0 and datum <= 0:
            result.append(np.nan)
        elif last > 0 and datum <= 0:
            result.append(last)
        else:
            result.append(datum)
            last = datum

    return result


df = pd.read_csv(os.path.join(DATA_ROOT, 'lengths.csv'), parse_dates=['date'])
files = [c for c in df.columns if c not in ['commit', 'date']]
df = df[df[files].sum(axis=1) > 0]
df = df.drop('commit', axis=1)
df = df.sort_values('date').set_index('date')
df = df.apply(
    lambda col: remove_zeros(col) if col.name not in ['commit', 'date'] else col
)

# Plot Emacs vs vim
fig, ax = plt.subplots(figsize=(12, 6))
df[['Emacs.org', 'init.vim', 'init.el']].plot(ax=ax)
ax.grid(True, alpha=0.25)
ax.set_axisbelow(True)
ax.set_title('Emacs vs neovim config size growth')
ax.set_ylabel('LoC')
ax.text(
    0.075,
    0.08,
    f'upd. {datetime.now().strftime("%Y-%m-%d")}',
    transform=fig.transFigure,
    va='top',
    ha='left'
)
plt.tight_layout()
fig.savefig(os.path.join(PICS_ROOT, 'emacs-vim.png'))

# Plot literate configuration files
fig, ax = plt.subplots(figsize=(12, 6))
df[['Emacs.org', 'Desktop.org', 'Mail.org', 'Guix.org',
    'Console.org']].plot(ax=ax)
ax.grid(True, alpha=0.25)
ax.set_axisbelow(True)
ax.set_title('Literate configuration size growth')
ax.set_ylabel('LoC')
ax.text(
    0.075,
    0.08,
    f'upd. {datetime.now().strftime("%Y-%m-%d")}',
    transform=fig.transFigure,
    va='top',
    ha='left'
)
plt.tight_layout()
fig.savefig(os.path.join(PICS_ROOT, 'literate-config.png'))
