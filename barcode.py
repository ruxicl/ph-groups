import matplotlib.pyplot as plt

INF = 2**30

def create_barcode(length, intervals):
    fig, ax = plt.subplots(figsize=(10, 6))
    
    for i, (start, end) in enumerate(intervals):
        if i == 0:
            col = "darkblue"
        else:
            col = "darkred"
        if end == INF:
            ax.hlines(y=i, xmin=start, xmax=length, color=col, lw=10)
            ax.plot([length, length + 0.15], [i, i], 
                    color=col, linestyle='-', linewidth=5, 
                    dashes=(1, 1))
        else: 
            ax.hlines(y=i, xmin=start, xmax=end, color=col, lw=10)

    ax.set_yticks([])
    ax.set_xticks(range(length + 1))
    ax.set_xlabel('Filtration step')
    ax.set_ylabel('Homology classes')
    plt.tight_layout()
    plt.show()
    
n = 5 # number of intervals
length = 5 # length of filtration
intervals = [(3, 5), (1,4),(2,5),(3,4),(3,4)] 

create_barcode(length, intervals)
