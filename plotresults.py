import matplotlib.pyplot as plt
#import matplotlib as mpl
from matplotlib.ticker import FormatStrFormatter

def prism_figure():
    prism_explicit = [0.041, 0.04, 0.029, 0.101, 0.308, 1.643, 33.557]
    prism_hybrid = [0.027, 0.047, 1.061, 53.463, 1768]
    prism_mtbdd = [0.049, 0.054, 1.087, 55.648, 1712]
    prism_sparse = [0.022, 0.045, 0.836, 57.858]


    plt.plot(range(2,9), prism_explicit, '-', color='#d62728', label="explicit", linewidth=2)
    plt.plot(range(2,7), prism_hybrid, '-.o', color='#2ca02c', label="hybrid")
    plt.plot(range(2,7), prism_mtbdd, ':', color='#1f77b4', label="mtbdd")
    plt.plot(range(2,6), prism_sparse, ':',  color='#ff7f0e', label="sparse")


    plt.text(8.15,35,"out-of-memory", color='#d62728')
    plt.text(6.2,1800,"time-out", color='#2ca02c')
    plt.text(6.2,1000,"time-out", color='#1f77b4')
    plt.text(5.2,60,"time-out", color='#ff7f0e')


    plt.xlim((1.5,9.99))
    plt.yscale('log')
    plt.xlabel('number of blocks')
    plt.ylabel('model construction time')

    plt.legend(loc='upper left')
    plt.show()

def storm_figure():
    storm_sparse = [0.026, 0.06, 0.069, 0.098, 0.244, 1.195, 9.406, 131.699]
    storm_hybrid = [0.006, 0.053, 1.37, 53.876]
    storm_dd = [0.008, 0.083, 2.169, 52.444]
    storm_abs = [0.005, 0.057, 1.424, 50.111]

    plt.plot(range(2,10), storm_sparse, '-', color='#d62728', label="sparse", linewidth=2)
    plt.plot(range(2,6), storm_hybrid, ':o', color='#ff7f0e', label="hybrid")
    plt.plot(range(2,6), storm_dd, ':',  color='#1f77b4', label="dd")
    plt.plot(range(2,6), storm_abs, '-.', color='#2ca02c', label="abs")

    plt.text(9,100,"out-of-memory", color='#d62728')
    plt.text(5.2,100,"time-out", color='#ff7f0e')
    plt.text(5.2,65,"time-out", color='#1f77b4')
    plt.text(5.2,40,"time-out", color='#2ca02c')


    plt.xlim((1.5,10.99))
    plt.yscale('log')
    plt.xlabel('number of blocks')
    plt.ylabel('model construction time')

    plt.legend(loc='upper left')
    plt.show()

def PRISM_STORM():
    prism_explicit = [0.041, 0.04, 0.029, 0.101, 0.308, 1.643, 33.557]
    prism_hybrid = [0.027, 0.047, 1.061, 53.463, 1768]
    prism_mtbdd = [0.049, 0.054, 1.087, 55.648, 1712]
    prism_sparse = [0.022, 0.045, 0.836, 57.858]

    plt.plot(range(2,9), prism_explicit, '-.', label="prism_explicit")
    plt.plot(range(2,7), prism_hybrid, '-.', label="prism_hybrid")
    plt.plot(range(2,7), prism_mtbdd, '-.', label="prism_mtbdd")
    plt.plot(range(2,6), prism_sparse, '-.', label="prism_sparse")

    storm_sparse = [0.026, 0.06, 0.069, 0.098, 0.244, 1.195, 9.406, 131.699]
    storm_hybrid = [0.006, 0.053, 1.37, 53.876]
    storm_dd = [0.008, 0.083, 2.169, 52.444]
    storm_abs = [0.005, 0.057, 1.424, 50.111]

    plt.plot(range(2,10), storm_sparse, ':', label="storm_sparse")
    plt.plot(range(2,6), storm_hybrid, ':', label="storm_hybrid")
    plt.plot(range(2,6), storm_dd, ':', label="storm_dd")
    plt.plot(range(2,6), storm_abs, ':', label="storm_abs")

    plt.text(8,300,"out-of-memory")
    plt.text(6.2,1500,"time-out")

    plt.xlim((1.01,10.99))
    plt.yscale('log')
    plt.xlabel('number of blocks')
    plt.ylabel('model construction time')

    plt.legend(loc='upper left')
    plt.show()


def pCTL_REBEL_figure():
    pCTL_REBEL = [0.017, 0.06, 0.093, 0.628, 2.791, \
                 10.454, 29.628, 65.728, 117.219, 185.591, \
                 249.961, 290.667, 316.301, 325.272, 323.516, \
                 323.773]
    prism_explicit_modelconstruction = [0.041, 0.04, 0.029, 0.101, 0.308, 1.643, 33.557]
    prism_explicit_modelchecking = [0.003, 0.007, 0.013, 0.034, 0.094, 0.86, 8.814]
    prism_explicit = [sum(i) for i in zip( prism_explicit_modelconstruction, prism_explicit_modelchecking)]
    prism_hybrid = [0.027, 0.047, 1.061, 53.463, 1768]
    prism_mtbdd = [0.049, 0.054, 1.087, 55.648, 1712]
    prism_sparse = [0.022, 0.045, 0.836, 57.858]

    storm_sparse_modelconstruction = [0.026, 0.06, 0.069, 0.098, 0.244, 1.195, 9.406, 131.699]
    storm_sparse_modelchecking = [0, 0, 0, 0, 0.006, 0.054, 1.017, 16.186]
    storm_sparse = [sum(i) for i in zip( storm_sparse_modelconstruction, storm_sparse_modelchecking)]
    storm_hybrid = [0.006, 0.053, 1.37, 53.876]
    storm_dd = [0.008, 0.083, 2.169, 52.444]
    storm_abs = [0.005, 0.057, 1.424, 50.111]


    plt.plot(range(2,18), pCTL_REBEL, '-', label="pCTL_REBEL")

    plt.plot(range(2,9), prism_explicit, '-.', label="prism_explicit")
    plt.plot(range(2,7), prism_hybrid, '-.', label="prism_hybrid")
    plt.plot(range(2,7), prism_mtbdd, '-.', label="prism_mtbdd")
    plt.plot(range(2,6), prism_sparse, '-.', label="prism_sparse")

    plt.plot(range(2,10), storm_sparse, ':', label="storm_sparse")
    plt.plot(range(2,6), storm_hybrid, ':', label="storm_hybrid")
    plt.plot(range(2,6), storm_dd, ':', label="storm_dd")
    plt.plot(range(2,6), storm_abs, ':', label="storm_abs")




    # plt.text(8.15,35,"out-of-memory", color='#d62728')
    # plt.text(9,100,"out-of-memory", color='#ff7f0e')

    plt.text(6.2,1500,"time-out")
    plt.text(7,200,"out-of-memory")

    # plt.xlim((1.5,10.99))
    plt.yscale('log')
    plt.xlabel('number of blocks')
    plt.ylabel('runtime')

    plt.legend(loc='lower right', frameon=False)
    plt.savefig('PRISM_STORM_pCTLREBEL.png')
    plt.show()

def make_patch_spines_invisible(ax):
    ax.set_frame_on(True)
    ax.patch.set_visible(False)
    for sp in ax.spines.values():
        sp.set_visible(False)

def blocksworld_limit():
    iters_recognition = [1, 2, 6, 7, 8, 9, 10, 12, 13, 15]
    iters_convergence = [5, 7, 12, 13, 14, 15, 16, 17, 19, 21]
    n_abstract_states = [2, 3, 10, 25, 45, 70, 100, 131, 165, 203]
    runtime = [0.029, 0.064, 0.811, 4.059, 17.658, 59.029, 156.272, 341.4, 736.632, 1457.974]


    fig, ax1 = plt.subplots()
    fig.subplots_adjust(right=0.75)
    ax1.set_xlabel('state bound: #blocks')
    ax1.set_ylabel("runtime")
    # ax1.set_yscale('log')
    p1, = ax1.plot(range(3,13), runtime, '-', color='#d62728', label="runtime")

    ax2 = ax1.twinx()
    ax2.set_ylabel("#iteration")
    ax2.yaxis.set_major_formatter(FormatStrFormatter('%d'))
    p2, = ax2.plot(range(3,13), iters_recognition, '-', color='#2ca02c', label="#iters recognition")
    p3, = ax2.plot(range(3,13), iters_convergence, '-', color='#1f77b4', label="#iters convergence")

    ax3 = ax1.twinx()
    ax3.set_ylabel("#abstract states")
    ax3.spines["right"].set_position(("axes", 1.2))
    make_patch_spines_invisible(ax3)
    ax3.spines["right"].set_visible(True)
    p4, = ax3.plot(range(3,13), n_abstract_states, '-', color='#ff7f0e', label="#abstract states")
    # fig.tight_layout()


    lines = [p1, p2, p3, p4]

    ax1.legend(lines, [l.get_label() for l in lines], frameon=False)
    plt.savefig('blocksworld_limit.png')
    plt.show()

def load_unload_limit():
    time_each_iter_5_cities = [0.013, 0.019,0.033,0.092,0.333,0.707,1.051,1.055,1.049,1.049,1.079,1.049,1.080]
    time_each_iter_13_cities = [0.016,0.047,0.132,0.725,4.088,11.529,41.849,79.752,\
                     99.032,112.224,101.082,100.445,101.199,99.858,99.897,99.525]
    time_iter_13 = []
    time_iter_5 = []
    sum_13 = 0
    sum_5 = 0
    for i in range(16):
        sum_13 += time_each_iter_13_cities[i]
        time_iter_13.append(sum_13)
    for i in range(13):
        sum_5 += time_each_iter_5_cities[i]
        time_iter_5.append(sum_5)


    n_abstract_states_13 = [2,5,14,34,58,108,145,160,170,170,170,170,170,170,170,170]
    n_abstract_states_5 = [2,3,7,14,21,26,26,26,26,26,26,26,26]

    fig, ax1 = plt.subplots()
    # fig.subplots_adjust(right=0.75)
    ax1.set_xlabel('#iterations')
    ax1.set_ylabel("runtime")
    # ax1.set_yscale('log')
    p1, = ax1.plot(range(1,17), time_iter_13, '-', color='#d62728', label="runtime (13 cities)")
    p3, = ax1.plot(range(1,14), time_iter_5, '-', color='#1f77b4', label="runtime (5 cities)")

    ax2 = ax1.twinx()
    ax2.set_ylabel("#abstract states")
    ax2.yaxis.set_major_formatter(FormatStrFormatter('%d'))
    p2, = ax2.plot(range(1,17), n_abstract_states_13, '-', color='#2ca02c', label="#abstract states (13 cities)")
    p4, = ax2.plot(range(1,14), n_abstract_states_5, '-', color='#ff7f0e', label="#abstract states (5 cities)")
    lines = [p1, p2, p3, p4]

    ax1.legend(lines, [l.get_label() for l in lines], frameon=False)
    plt.savefig('load_unload_limit.png')
    plt.show()


def limit():
    n_abstract_states_blocksworld = [2, 3, 10, 25, 45, 70, 100, 131, 165, 203]
    runtime_blocksworld = [0.029, 0.064, 0.811, 4.059, 17.658, 59.029, 156.272, 341.4, 736.632, 1457.974]

    n_abstract_states_load_unload = [10,17,26,37,50,65,82,101,122,145,170]
    runtime_load_unload = [1.029,3.534,8.614,18.063,41.938,79.615,142.557,260.566,469.848,724.542,951.409]

    fig, ax1 = plt.subplots()
    # fig.subplots_adjust(right=0.75)
    ax1.set_xlabel('state bound (blocksworld) / #cities (load-unload)')
    ax1.set_ylabel("runtime")
    # ax1.set_yscale('log')
    p1, = ax1.plot(range(3,13), runtime_blocksworld, '-', color='#d62728', label="blockworld runtime")
    p2, = ax1.plot(range(3,14), runtime_load_unload, '-', color='#2ca02c', label="load unload runtime ")

    ax3 = ax1.twinx()
    ax3.set_ylabel("#abstract states")
    p3, = ax3.plot(range(3,13), n_abstract_states_blocksworld, '-', color='#1f77b4', label="blockworld #abstract states")
    p4, = ax3.plot(range(3,14), n_abstract_states_load_unload, '-', color='#ff7f0e', label="load unload #abstract states")

    lines = [p1, p2, p3, p4]

    ax1.legend(lines, [l.get_label() for l in lines], frameon=False)
    plt.savefig('limit.png')
    plt.show()

def stepbound():
    abstract_states_inner = [97, 97, 199, 287]
    runtime_inner = [66.99, 67.769, 721.208, 4209.868]

    abstract_states_outer = [9,25,54,97,151]
    runtime_outer = [0.183,1.638,14.834,67.769,269.674]


    plt.locator_params(axis='x', nbins=5)
    plt.xlabel('step bound')
    plt.ylabel("runtime")
    # ax1.set_yscale('log')
    p1, = plt.plot(range(1, 5), runtime_inner, '-', color='#d62728', label="inner runtime")
    p2, = plt.plot(range(1, 6), runtime_outer, '-', color='#2ca02c', label="outer runtime")

    plt.legend(frameon=False)
    plt.savefig('stepbound.png')
    plt.show()

# PRISM_STORM()
# prism_figure()
# storm_figure()
# pCTL_REBEL_figure()
# blocksworld_limit()
# load_unload_limit()
# limit()
stepbound()
