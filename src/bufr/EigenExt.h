//
// https://eigen.tuxfamily.org/dox/TopicCustomizing_NullaryExpr.html
//

#pragma once

#include "Eigen/Dense"

using namespace Eigen;


/// Add Matlab style array indexing
template<class ArgType, class RowIndexType, class ColIndexType>
class indexing_functor {
    const ArgType &m_arg;
    const RowIndexType &m_rowIndices;
    const ColIndexType &m_colIndices;
public:
    typedef Array<typename ArgType::Scalar,
        RowIndexType::SizeAtCompileTime,
        ColIndexType::SizeAtCompileTime,
        ArgType::Flags&RowMajorBit?RowMajor:ColMajor,
        RowIndexType::MaxSizeAtCompileTime,
        ColIndexType::MaxSizeAtCompileTime> ArrayType;

    indexing_functor(const ArgType& arg, const RowIndexType& row_indices, const ColIndexType& col_indices) :
        m_arg(arg),
        m_rowIndices(row_indices),
        m_colIndices(col_indices)
    {
    }

    const typename ArgType::Scalar& operator() (Index row, Index col) const
    {
        return m_arg(m_rowIndices[row], m_colIndices[col]);
    }
};

template <class ArgType, class RowIndexType, class ColIndexType>
CwiseNullaryOp<indexing_functor<ArgType,RowIndexType,ColIndexType>, typename indexing_functor<ArgType,RowIndexType,ColIndexType>::ArrayType>
indexing(const Eigen::ArrayBase<ArgType>& arg, const RowIndexType& row_indices, const ColIndexType& col_indices)
{
    typedef indexing_functor<ArgType,RowIndexType,ColIndexType> Func;
    typedef typename Func::ArrayType ArrayType;
    return ArrayType::NullaryExpr(row_indices.size(), col_indices.size(), Func(arg.derived(), row_indices, col_indices));
}
